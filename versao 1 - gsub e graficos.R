# Projeto ME115 - Analise de franquias de filmes

# The Numbers Budget
library(XML)
library(methods)
library(jsonlite)
library(plyr)
library(stringr)
if (!require(gsubfn)) install.packages('gsubfn')
library(gsubfn)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(RColorBrewer)

# Tabela do site
u <- "http://www.the-numbers.com/movie/budgets/all/1"
tables <- readHTMLTable(u)
TheNumbers <- tables[[1]]

j <- 101
link <- "http://www.the-numbers.com/movie/budgets/all/"
while (j<5402){
  k <- paste(link,j, sep="")
  tables <- readHTMLTable(k)
  tables2 <- tables[[1]]
  TheNumbers <- rbind(TheNumbers,tables2)
  j <- j+100
}

# Deletar linhas NA
for (i in 1:nrow(TheNumbers)) {
  if(is.na(TheNumbers$Movie[i])==TRUE){
    TheNumbers <- TheNumbers[-i,]
  }
}

# Arrumando indice TheNumbers
rownames(TheNumbers) <- 1:nrow(TheNumbers)
TheNumbers[,1] <- c()

# Mudando str
TheNumbers$Movie <- as.character(TheNumbers$Movie)
TheNumbers$`Release Date` <- as.character(TheNumbers$`Release Date`)
TheNumbers$`Production Budget` <- as.character(TheNumbers$`Production Budget`)
TheNumbers$`Domestic Gross` <- as.character(TheNumbers$`Domestic Gross`)
TheNumbers$`Worldwide Gross` <- as.character(TheNumbers$`Worldwide Gross`)

# The Numbers Franquias
u2 <- 'http://www.the-numbers.com/movies/franchises/'
tablesFranquia <- readHTMLTable(u2)
Franquias <- tablesFranquia[[1]]
Franquias$`No. of Movies` <- as.character(Franquias$`No. of Movies`)
Franquias$`No. of Movies` <- as.numeric(Franquias$`No. of Movies`)

# The Numbers Franquias Amostra 
FranquiasAmostra <- read.csv("Controle de banco de dados - Tabela Amostra.csv")
FranquiasAmostra <- FranquiasAmostra[,-c(1,2)]

# Banco de dados OMDB
# Arquivo JSON txt para table
filmes1 <- readLines("filmesJoao.txt")
filmes2 <- readLines("filmesJu.txt")
filmes3 <- readLines("filmesPera.txt")
filmes4 <- readLines("filmesIgnacio.txt")

filmesOMDB <- rbind(fromJSON(filmes1),fromJSON(filmes2),fromJSON(filmes3),fromJSON(filmes4))


# Retirar filmes duplicados
filmesDuplicados <- c(260,267,268,269,337,463,530)
filmesOMDB <- filmesOMDB[-filmesDuplicados,]
rownames(filmesOMDB) <- 1:nrow(filmesOMDB)

# Vetor com os titulos dos filmes
TitleOMDB <- filmesOMDB$Title
TitleTN <- TheNumbers$Movie
equalNamesOMDB <- c()
equalNamesTN <- c()
difNamesOMDB <- c()

# Deletando colunas de nao interesse
filmesOMDB <- filmesOMDB[,-c(7,8,9,10,14,19,20,21,24,25)]

# Indice filme OMDB
indiceOMDB <- c(1:nrow(filmesOMDB))

# Ratings Sources
# Tabela dos ratings
Ratings <- data.frame(filmesOMDB$Ratings[1],indiceOMDB[1])
colnames(Ratings) <- c("Source","Value","Index")
for (i in 2:nrow(filmesOMDB)){
  if (filmesOMDB$Ratings[i] != "list()"){
    Ratings1 <- data.frame(filmesOMDB$Ratings[i],indiceOMDB[i])
    colnames(Ratings1) <- c("Source","Value","Index")
    Ratings <- rbind(Ratings,Ratings1)
  }
}

# Filmes com avaliacao Tomatoes
filmesTomatoes <- c()
ratTomatoes <- c()
for (i in 1:nrow(Ratings)){
  if (Ratings$Source[i]=="Rotten Tomatoes"){
    filmesTomatoes <- append(filmesTomatoes,Ratings$Index[i])
    ratTomatoes <- append(ratTomatoes,Ratings$Value[i])
  }
}

# Adicionando coluna Tomatoes a filmesOMDB
filmesOMDB$Tomatoes <- c()
for (i in 1:nrow(filmesOMDB)){
  if (i==1){
    filmesOMDB$Tomatoes[1] <- "N/A"
  }
  filmesOMDB$Tomatoes[filmesTomatoes[i]] <- ratTomatoes[i]
}

# Substituindo N/A por NA
for (i in 1:nrow(filmesOMDB)){
  for (j in 1:ncol(filmesOMDB)){
    if (filmesOMDB[i,j]=="N/A"){
      filmesOMDB[i,j] <- NA
    }
  }
}

# Reordenando colunas
filmesOMDB <- filmesOMDB[,c(1:13,16,14:15)]

# Datas
for (i in 1:nrow(filmesOMDB)){
  filmesOMDB$Released[i] <- as.character(as.Date(filmesOMDB$Released[i], format= "%d %B %Y"))
}

for (i in 1:nrow(TheNumbers)){
  TheNumbers$`Release Date`[i] <- as.character(as.Date(TheNumbers$`Release Date`[i], format= "%m/%d/%Y"))
}

# Franquias dados nao encontrados
franquiasErro <- c("Detective Dee", "Angel", "Tiger & Bunny", "Belle de Jour", "Boondock Saints", "Blade Runner", "Project X", "Hancock", "Weekend at Bernie’s", "Dark Universe", "Un homme et une femme", "Bad Grandpa", "Tom yum goong","On the Run","Kingsman","The Incredibles","Laura Cadieux","Snabba Cash","Gnomeo and Juliet","Tales from the Crypt","The Land Before Time","Jeepers Creepers","Chinatown", "Arthur and the Minimoys", "Wallace and Gromit")


# Filmes com nomes iguais (Remakes)
for(i in 1:nrow(filmesOMDB)){
  filme <- filmesOMDB$Title[i]
  for(j in (i+1):nrow(filmesOMDB)){
    if(filme == filmesOMDB$Title[j]){
      filmesOMDB$Title[j] <- paste(filmesOMDB$Title[j], "_R")
    }
  }
}

linkFranquia <- 'http://www.the-numbers.com/movies/franchise/'
tabelaFr <- '#tab=summary'
FranquiaBudget <- c()
indicesI <- c()

for (i in 1:nrow(FranquiasAmostra)){
  apost <- c(27, 148, 189, 48, 51, 177, 179)
  filmesThe <- c(59,71,72,94,104,114,124)
  if (i %in% apost) {
    nomeFranquia <- gsub("'","",as.character(FranquiasAmostra$Franchise[i]))
    nomeFranquia <- gsub("An Inconvenient Truth", "Inconvenient Truth An",nomeFranquia)
    nomeFranquia <- gsub(" 2007", "", nomeFranquia)
    nomeFranquia <- gsub("\\[", "", nomeFranquia)
    nomeFranquia <- gsub("\\]","", nomeFranquia)
    nomeFranquia <- gsub("A Fish Called Wanda", "Fish Called Wanda A",nomeFranquia)
  } else { 
    if (i %in% filmesThe) {
      nomeFranquia <- gsub("^The ", "", FranquiasAmostra$Franchise[i])
      nomeFranquia <- paste(nomeFranquia, "The")
    } else {
    indicesI <- append(indicesI,i)
    nomeFranquia <- gsubfn(".",list("."="", ","="","/"="","&"="and","?"="","'"=" "),as.character(FranquiasAmostra$Franchise[i]))
  }
  nomeFranquia <- gsub(" ", "-", nomeFranquia)
  linkFranquiaNome <- paste0(linkFranquia,nomeFranquia,tabelaFr)
  tablesFr <- readHTMLTable(linkFranquiaNome)
  tablesFr2 <- tablesFr[[1]]
  FranquiaBudget <- rbind(FranquiaBudget, tablesFr2)
} }


# Removendo colunas e linhas desencessarias
FranquiaBudget <- FranquiaBudget[,c(-1,-7)]
for (i in 1:nrow(FranquiaBudget)) {
  if(is.na(FranquiaBudget[i,1])==TRUE){
    FranquiaBudget <- FranquiaBudget[-i,]
  } 
}
rownames(FranquiaBudget) <- 1:nrow(FranquiaBudget)

# Remakes
FranquiaBudget$Movie <- as.character(FranquiaBudget$Movie)
for(i in 1:nrow(FranquiaBudget)){
  filmefr <- FranquiaBudget$Movie[i]
  for(j in (i+1):nrow(FranquiaBudget)){
    if(filmefr == FranquiaBudget$Movie[j]){
      FranquiaBudget$Movie[j] <- paste(FranquiaBudget$Movie[j], "_R")
    }
  }
}

# Mudança de Nomes ----------------------------------

FranquiaBudget$Movie[489] <- "Three Colors: Blue"
FranquiaBudget$Movie[490] <- "Three Colors: White"
FranquiaBudget$Movie[491] <- "Three Colors: Red"
FranquiaBudget$Movie[496] <- "Casse-tÍte chinois pour le judoka"
FranquiaBudget$Movie[509] <- "Van Wilder: Party Liaison"
FranquiaBudget$Movie[510] <- "Van Wilder 2: The Rise of Taj"
FranquiaBudget$Movie[531] <- "Joy Ride 3: Road Kill"
FranquiaBudget$Movie[534] <- "Pee-wee's Big Adventure"
FranquiaBudget$Movie[541] <- "The Sandlot: Heading Home"
FranquiaBudget$Movie[689] <- "The Marine 2"
FranquiaBudget$Movie[690] <- "The Marine 3: Homefront"
FranquiaBudget$Movie[691] <- "The Marine 4: Moving Target"
FranquiaBudget$Movie[697] <- "Jaws: The Revenge"
FranquiaBudget$Movie[746] <- "Pirates of the Caribbean: Dead Man's Chest"
FranquiaBudget$Movie[747] <- "Pirates of the Caribbean: At World's End"
FranquiaBudget$Movie[773] <- "Underworld: Rise of the Lycans"
FranquiaBudget$Movie[774] <- "Underworld Awakening"
FranquiaBudget$Movie[829] <- "Chill Out, Scooby-Doo!"
FranquiaBudget$Movie[990] <- "Guardians of the Galaxy Vol. 2"
FranquiaBudget$Movie[330] <- "Best of the Best II"
FranquiaBudget$Movie[358] <- "Cirque du Soleil: Journey of Man"
FranquiaBudget$Movie[387] <- "The Swan Princess: Escape from Castle Mountain"
FranquiaBudget$Movie[411] <- "The Huntsman: Winter's War"
FranquiaBudget$Movie[655] <- "Blade II"
FranquiaBudget$Movie[660] <- "New Year's Eve"
FranquiaBudget$Movie[661] <- "Mother's Day"
FranquiaBudget$Movie[894] <- "The Twilight Saga: Breaking Dawn - Part 1"
FranquiaBudget$Movie[895] <- "The Twilight Saga: Breaking Dawn - Part 2"
FranquiaBudget$Movie[907] <- "An American Girl Holiday"
FranquiaBudget$Movie[908] <- "An American Girl Adventure"
FranquiaBudget$Movie[909] <- "An American Girl on the Home Front"
FranquiaBudget$Movie[912] <- "Grace Stirs Up Success"
FranquiaBudget$Movie[870] <- "It's the Easter Beagle, Charlie Brown!"
FranquiaBudget$Movie[872] <- "Bon Voyage, Charlie Brown (and Don't Come Back!!)"
FranquiaBudget$Movie[877] <- "Happiness Is a Warm Blanket, Charlie Brown"


# fim ------------------------------------------------------

# Vetor com os titulos dos filmes
TitleOMDB <- filmesOMDB$Title
TitleFranquias <- FranquiaBudget$Movie
equalNamesOMDB <- c()
equalNamesFranquia <- c()

# Encontrando nomes iguais
for (i in 1:length(TitleOMDB)){
  for (j in 1:length(TitleFranquias)){
    if (toupper(TitleOMDB[i])==toupper(TitleFranquias[j])) {
      equalNamesOMDB <- append(equalNamesOMDB, i)
      equalNamesFranquia <- append(equalNamesFranquia, j)
    } 
  }
}

# Filmes com nomes diferentes
filmesMudar <- c(1:nrow(filmesOMDB))
filmesMudar <- filmesMudar [! filmesMudar %in% equalNamesOMDB]


# Tranformando colunas em valores numerics
for (i in 1:nrow(filmesOMDB)) {
  for (j in c(2,5,11,12,13,14,15)) {
    if (j==5) {
      filmesOMDB[i,j] <- as.numeric(gsub(" min","",filmesOMDB[i,j]))
    }
    if (!is.na(filmesOMDB[i,j])) {
      filmesOMDB[i,j] <- as.numeric(gsubfn(".",list("$"="",","="","%"=""),filmesOMDB[i,j]))
    }
  }
}

for (j in c(2,5,11,12,13,14,15)) {
  filmesOMDB[,j] <- as.numeric(filmesOMDB[,j])
}

for (i in 1:nrow(FranquiaBudget)) {
  for (j in c(2,3,4,5)) {
    FranquiaBudget[,j] <- as.character(FranquiaBudget[,j])
    if (!is.na(FranquiaBudget[i,j])) {
      FranquiaBudget[i,j] <- gsubfn(".",list("$"="",","="","%"=""),FranquiaBudget[i,j])
    }
  }
}

for (j in c(2,3,4,5)) {
  FranquiaBudget[,j] <- as.numeric(FranquiaBudget[,j])
}

# Juntando dados
filmesOMDB$Budget <- NA
filmesOMDB$domGross <- NA
filmesOMDB$wwGross <- NA
for (i in 1:length(equalNamesOMDB)){
  filmesOMDB$Budget[equalNamesOMDB[i]] <- FranquiaBudget$ProductionBudget[equalNamesFranquia[i]]
  filmesOMDB$domGross[equalNamesOMDB[i]] <- FranquiaBudget$`DomesticBox Office`[equalNamesFranquia[i]]
  filmesOMDB$wwGross[equalNamesOMDB[i]] <- FranquiaBudget$`WorldwideBox Office`[equalNamesFranquia[i]]
}

# The Number valores numericos
for (i in 1:nrow(TheNumbers)) {
  for (j in c(3,4,5)) {
    if (!is.na(TheNumbers[i,j])) {
      TheNumbers[i,j] <- gsubfn(".",list("$"="",","="","%"=""),TheNumbers[i,j])
    }
  }
}

for (j in c(3,4,5)) {
  TheNumbers[,j] <- as.numeric(TheNumbers[,j])
}

# No de filmes em FranquiaBudget
FranquiaBudget$numMovies <- NA
n <- 0
i <- 1
while (i<=nrow(FranquiaBudget)) {
  n <- n+1
  if (grepl("Totals",FranquiaBudget$Movie[i])) {
    FranquiaBudget$numMovies[i] <- n
    n <- 0
    i <- i+2
  }
  i <- i+1
}
FranquiaBudget$numMovies[3] <- 2

franquiasTotal <- subset(FranquiaBudget,!is.na(FranquiaBudget$numMovies))
franquiasTotal <- franquiasTotal[,-3]

# Transformando 0 em NA
for (i in 1:nrow(filmesOMDB)) {
  for (j in c(17,18,19)) {
    if (!is.na(filmesOMDB[i,j])) {
      if (filmesOMDB[i,j]==0) {
        filmesOMDB[i,j] <- NA
    }
    }
  }
}


# GRAFICOSSSSSSS
BudgetMAIORZERO <- subset(filmesOMDB, filmesOMDB$Budget>0)
wwGrossMAIORZERO <- subset(filmesOMDB, filmesOMDB$wwGross>0)
domGrossMAIORZERO <- subset(filmesOMDB, filmesOMDB$domGross>0)

# HEAT MAP
filmesHEATMAP <- filmesOMDB[,c(1,17,18,19)]
filmes.m <- melt(filmesHEATMAP)
ggplot(filmes.m, aes(variable, Title)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(low = "lightyellow",high = "darkblue", na.value = "red") + ylab("Filme") + xlab("") 

filmesHEATMAP2 <- filmesOMDB[,c(1,11,12,13,14)]
filmes.m2 <- melt(filmesHEATMAP2)
filmes.m2 <- ddply(filmes.m2, .(variable), transform,rescale = scale(value))
ggplot(filmes.m2, aes(variable, Title)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "lightyellow",high = "darkblue", na.value = "red") + ylab("Filme") + xlab("") 

# The Numbers (Filmes em geral)
ggplot(TheNumbers, aes(x=`Production Budget`, y=`Worldwide Gross`)) + geom_point(size=2, alpha=0.7,col="black") + geom_smooth(method=lm,alpha=0.3, col="darkorchid1", fill="darkorchid1") + xlab("Receita ($)") + ylab("Bilheteria Mundial ($)")

# Histograma
h1 <- ggplot(BudgetMAIORZERO,aes(x=Budget)) + geom_histogram(col="white", fill="black", alpha=0.8) + xlab("Receita ($)") + ylab("Frequencia")
ggplot(wwGrossMAIORZERO,aes(x=wwGross)) + geom_histogram(col="white", fill="black", alpha=0.8) + xlab("Bilheteria Mundial ($)") + ylab("Frequencia")
ggplot(domGrossMAIORZERO,aes(x=domGross)) + geom_histogram(col="white", fill="black", alpha=0.8) + xlab("Bilheteria US ($)") + ylab("Frequencia")
hist(filmesOMDB$Runtime, breaks=200, probability = TRUE)
lines(density(filmesOMDB$Runtime, na.rm = TRUE), col="red", lwd=1.5)
ggplot(filmesOMDB, aes(y=Runtime)) + geom_boxplot()


# BoxOffice x wwGross
ggplot(filmesOMDB,aes(x=Budget,y=wwGross)) + geom_point(size=5, alpha=0.7, aes(fill=as.factor(Genre), col=as.factor(Genre))) + theme(legend.position = "none") + xlab("Receita ($)") + ylab("Bilheteria Mundial ($)") + geom_smooth(method=lm,alpha=0.2, col="black") 
ggplot(filmesOMDB,aes(x=Budget,y=wwGross, fill=cut(Year, 10), col=cut(Year,10))) + geom_point(size=5, alpha=0.7)  + xlab("Receita ($)") + ylab("Bilheteria Mundial ($)") + geom_smooth(method=lm, alpha=0.2)
ggplot(filmesOMDB,aes(x=Budget,y=wwGross, fill=cut(Year, 10), col=cut(Year,10))) + geom_point(size=5, alpha=0.7) + facet_wrap(~cut(Year,8), ncol=4) + xlab("Receita ($)") + ylab("Bilheteria Mundial ($)")

# BoxOffice x domGross
ggplot(filmesOMDB,aes(x=Budget,y=domGross, fill=as.factor(Genre), col=as.factor(Genre))) + geom_point(size=5, alpha=0.7) + theme(legend.position = "none") + xlab("Receita ($)") + ylab("Bilheteria US ($)") 
ggplot(filmesOMDB,aes(x=Budget,y=domGross, fill=cut(Year, 10), col=cut(Year,10))) + geom_point(size=5, alpha=0.7)  + xlab("Receita ($)") + ylab("Bilheteria US ($)") + geom_smooth(method=lm, alpha=0.2)

# wwGross x Tomatoes
ggplot(wwGrossMAIORZERO,aes(x=Tomatoes,y=wwGross, fill=as.factor(Genre), col=as.factor(Genre))) + geom_point(size=5, alpha=0.7) + theme(legend.position = "none") + xlab("Tomatoes (%)") + ylab("Bilheteria Mundial ($)") 
ggplot(wwGrossMAIORZERO,aes(x=Tomatoes,y=wwGross, fill=cut(Year, 10), col=cut(Year,10))) + geom_point(size=5, alpha=0.7)  + xlab("Tomatoes (%)") + ylab("Bilheteria US ($)") + geom_smooth(method=lm, alpha=0.2)

# Total Franquias
ggplot(franquiasTotal,aes(x=ProductionBudget,y=`WorldwideBox Office`, col=as.factor(numMovies), fill=as.factor(numMovies))) + geom_point(size=5, alpha=0.7) + facet_wrap(~cut(numMovies,7),ncol=3)

# Funçao tirar codigos de pont (deixar guardado)
#q <- "Gekij<U+014D>ban Poketto Monsut<U+0101> Adobansu Jener<U+0113>shon Rekk<U+016B> no H<U+014D>monsha Deokishisu"
#gsub("\\s*<U\\+\\w+>\\s*", "", q)

