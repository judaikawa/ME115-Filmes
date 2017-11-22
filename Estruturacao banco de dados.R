# OMDb API: 4c8894e7
#TMDb API: cef2bcdc45ae79353efb22c4bc10f3bd

devtools::install_github("hrbrmstr/omdbapi")
library(omdbapi)
library(TMDb)
library(rvest)
library(magrittr)
library(ggplot2movies)
library(plyr)
library(R.utils)

# Filmes IMDb por ggplot2movies (excluindo shorts) ------------------------
filmesggplot2 <- data.frame(movies)
filmesggplot2 <- subset(filmesggplot2, filmesggplot2$Short==0)
particulas <- c(", The", ", A", ", An", ", El", ", Les", ", Los", ", Las", ", Un", ", Uno", ", Una", ", Le", ", La", ", O", ", Der", ", Die", ", De", ", Das", ", Den")

# Alterando nome de filmes com problemas -----------------------------------
for (i in 1:nrow(movies)) {
  for (j in 1:length(particulas)) {
    if (grepl(particulas[j], filmesggplot2$title[i])) {
      filmesggplot2$title[i] <- paste(gsub(", ", "", particulas[j]), gsub(particulas[j], "", filmesggplot2$title[i]))
    }
    if (grepl(", L'", filmesggplot2$title[i])) {
      filmesggplot2$title[i] <- paste0("L'", gsub(", L'", "", filmesggplot2$title[i]))
    }
  }
}
rownames(filmesggplot2) <- 1:nrow(filmesggplot2)

# Filmes IMDB Mais Populares entre 1900 at?? 2017 ------------------------------
imdbID <- c()
for (i in 1:100) {
  print(i)
  url <- paste0('http://www.imdb.com/search/title?count=100&release_date=1900,2017&title_type=feature&view=advanced&page=',i,'&ref_=adv_nxt')
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'.lister-item-header a')
  imdbID_data <- data_html %>% html_attr('href')
  imdbID_data <- unlist(strsplit(imdbID_data, "/"))
  imdbID <- append(imdbID,imdbID_data[grep('^tt', imdbID_data)])
}

# Tabela IMDb ---------------------------------------------------------------
filmesIMDB <- data.frame()
filmeBudget <- c()
filmeWWGross <- c()
filmeDomGross <- c()
filmesErro <- c()

j <- 9953
while (j <= 10000) {
  tryCatch({
    filme <- cbind(find_by_id(imdbID[j],plot="full"),j)
    if (length(filme$Ratings)==0) {
      filme$Ratings <- 0
    }
    filmesIMDB <- rbind.fill(filmesIMDB, filme)
    filmeHTML2 <- read_html(paste0("http://www.imdb.com/title/",imdbID[j],"/business"))
    grossSTR <- as.character(html_text(html_node(filmeHTML2,paste0('#tn15content'))))
    grossSTR2 <- strsplit(grossSTR,'\\$')
    grossSTR2 <- unlist(grossSTR2)
    if (length(grossSTR2[grep("^\n \n\nBudget\n$",grossSTR2)])>0) {
      filmeBudget <- append(filmeBudget,strsplit(grossSTR2[grep("Budget",grossSTR2)+1][1]," ")[[1]][1])
    } else {
      filmeBudget <- append(filmeBudget,NA)
    }
    if (length(grossSTR2[grep("Worldwide",grossSTR2)])>0) {
      filmeWWGross <- append(filmeWWGross,strsplit(grossSTR2[grep("Worldwide",grossSTR2)][1]," ")[[1]][1])
    } else {
      filmeWWGross <- append(filmeWWGross,NA)
    }
    if (length(grossSTR2[grep("\\(USA)",grossSTR2)])>0) {
      trueFalse <- grep("USA",grossSTR2)>grep("Gross",grossSTR2)[1]
      filmeDomGross <- append(filmeDomGross,strsplit(grossSTR2[grep("USA",grossSTR2)][min(which(trueFalse == TRUE))]," ")[[1]][1])
    } else {
      filmeDomGross <- append(filmeDomGross,NA)
    }
    print(j)
    j= j+1
  }, error=function(e) {
    print(j)
    print(e)
  })
}

# Trailer dos Filmes --------------------------------------------------------
TMDbKey <- "cef2bcdc45ae79353efb22c4bc10f3bd"
trailerKeys <- c()
filmesSemTrailer <- c()

i <- 9967
while (i <=10000) {
  tryCatch({
    trailerVideos <- movie_videos(TMDbKey, imdbID[i])$results
    if (!is.null(subset(trailerVideos, trailerVideos$type=="Trailer")$key[1])) {
      trailerKeys[i-8000] <- subset(trailerVideos, trailerVideos$type=="Trailer")$key[1]
    } else {
      trailerKeys[i-8000] <- NA
    }
    print(i)
    i <- i+1
  }, error=function(e){
    print(i)
    print(e)
  })
}

# Ratings Sources -----------------------------------------------------------
# Tabela dos ratings
Ratings <- data.frame()
for (i in 1:nrow(filmesIMDB)) {
  Ratings <- rbind.fill(Ratings,data.frame(filmesIMDB$Ratings[i], filmesIMDB$j[i]))
}

# Filmes com avaliacao Tomatoes ---------------------------------------------
filmesTomatoes <- c()
ratTomatoes <- c()
for (i in 1:nrow(Ratings)){
  if (!is.na(Ratings$Source[i])) {
    if (Ratings$Source[i]=="Rotten Tomatoes"){
      filmesTomatoes <- append(filmesTomatoes,Ratings$filmesIMDB.j.i.[i])
      ratTomatoes <- append(ratTomatoes,as.character(Ratings$Value[i]))
    }
  }
}

# Tabela IMDb 1 linha por filme -----------------------------------------------
linhasRemover <- c()
for (i in 2:nrow(filmesIMDB)) {
  if (filmesIMDB$j[i]==filmesIMDB$j[i-1]) {
    linhasRemover <- append(linhasRemover,i)
  }
}
filmesIMDB <- filmesIMDB[-linhasRemover,]
rownames(filmesIMDB) <- 1:nrow(filmesIMDB)

# Adicionando coluna Tomatoes e Budget a filmesIMDB ---------------------------
filmesIMDB$Tomatoes <- c()
for (i in 1:length(filmesTomatoes)) {
  if (i==1) {
    filmesIMDB$Tomatoes[1] <- NA
  }
  filmesIMDB$Tomatoes[which(filmesIMDB$j==filmesTomatoes[i])] <- ratTomatoes[i]
}
filmesIMDB <- cbind(filmesIMDB, filmeBudget, filmeWWGross, filmeDomGross, trailerKeys)

# Substituindo N/A por NA ----------------------------------------------------
for (i in 1:nrow(filmesIMDB)){
  for (j in 1:ncol(filmesIMDB)){
    if (!is.na(as.character(filmesIMDB[i,j]))) {
      if (as.character(filmesIMDB[i,j])=="N/A"){
        filmesIMDB[i,j] <- NA
      }
    }
  }
}

