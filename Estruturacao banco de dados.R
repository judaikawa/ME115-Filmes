# OMDb API: 4c8894e7

devtools::install_github("hrbrmstr/omdbapi")
library(omdbapi)
library(rvest)
library(magrittr)
library(ggplot2movies)
library(plyr)

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

# Tabela IMDb ---------------------------------------------------------------
rownames(filmesggplot2) <- 1:nrow(filmesggplot2)
filmesIMDB <- data.frame()
filmeBudget <- c()
filmeWWGross <- c()
filmeDomGross <- c()
filmesErro <- c()

for (j in 1:nrow(filmesggplot2)) {
  tryCatch({
    filme <- cbind(find_by_title(filmesggplot2$title[j]),j)
  }, finally = {
    if (!j %in% filmesErro) {
      if (length(filme$Ratings)==0) {
        filme$Ratings <- 0
      }
      filmesIMDB <- rbind.fill(filmesIMDB, filme)
      filmeHTML2 <- read_html(paste0("http://www.imdb.com/title/",filmesIMDB$imdbID[nrow(filmesIMDB)],"/business"))
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
    }
  }, error=function(e) {
    print(paste("Erro",j))
    filmesErro <<- append(filmesErro, j)
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
filmesIMDB <- cbind(filmesIMDB, filmeBudget, filmeWWGross, filmeDomGross)

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

