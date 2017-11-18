load("filmes_1_ate_5180")
library(ggplot2)
filmesIMDB$filmeDomGross <- as.numeric(gsub(",","",filmesIMDB$filmeDomGross))
filmesIMDB$filmeDomGross[is.na(filmesIMDB$filmeDomGross)] <- 0
filmesIMDB$filmeBudget <- as.numeric(gsub(",","",filmesIMDB$filmeBudget))*10^(-6)
filmesIMDB$filmeWWGross <- as.numeric(gsub(",","",filmesIMDB$filmeWWGross))
filmesIMDB$filmeWWGross[is.na(filmesIMDB$filmeWWGross)] <- 0
filmesIMDB$Year <- as.numeric(filmesIMDB$Year)

#função para criarmos uma coluna de bilheteria
Bilheteria <- list()
for (i in 1:nrow(filmesIMDB)){
  x <- (filmesIMDB[i,30] + filmesIMDB[i,31])*10^(-6)
  Bilheteria <- unlist(append(Bilheteria, x))
}
Bilheteria <- round(Bilheteria, 3)
Bilheteria
Bilheteria[Bilheteria == 0] <- NA
filmesIMDB["Bilheteria"] <- Bilheteria
filmesIMDB$filmeDomGross[filmesIMDB$filmeDomGross == 0] <- NA
filmesIMDB$filmeWWGross[filmesIMDB$filmeWWGross == 0] <- NA
#funçao para usarmos um genero
genero <- list()
for (i in 1:nrow(filmesIMDB)){
  a <- strsplit(filmesIMDB[i, 6], ",")
  genero <- append(genero, a[[1]][1])
}
genero
filmesIMDB$Genre <- genero

#funçao para usarmos os premios
premio <- list()
for (i in 1:nrow(filmesIMDB)){
  if (grepl("win", filmesIMDB[i,13]) == TRUE){
    premio <- append(premio, 1)
  }
  else{
    premio <- append(premio, 0)
  }
}
premio
filmesIMDB$Awards <- premio


#funçao para usarmos as notas do Rotten
rotten <- list()
for (i in 1:nrow(filmesIMDB)){
  b <- strsplit(as.character(filmesIMDB[i, 28]), "\\%")
  rotten <- append(rotten, b[[1]][1])
}
rotten
filmesIMDB$Tomatoes <- as.numeric(unlist(rotten)) 

#função para plotar gráficos parelhos
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# testes envolvendo bilheteria
## correlação entre ano e bilheteria
plot1 <- ggplot(filmesIMDB, aes(x = Year, y = Bilheteria)) + geom_point()
plot1
plot1.genero <- ggplot(filmesIMDB, aes(x = Year, y = Bilheteria, col = as.factor(unlist(Genre)))) + geom_point() + facet_wrap(~as.factor(unlist(Genre)), 5) + theme(legend.position = "none")
plot1.genero

## correlação entre orçamento e bilheteria
plot2 <- ggplot(filmesIMDB, aes(filmeBudget, Bilheteria)) + geom_point()
plot2
plot2.premios <- ggplot(filmesIMDB, aes(filmeBudget, Bilheteria, col = as.factor(unlist(Awards)))) + geom_point() 
plot2.premios
plot2.premios2 <- plot2.premios + facet_grid(~as.factor(unlist(Awards)), 5)+ theme(legend.position = "none")
plot2.premios2
plot2.bp.premios <- boxplot(filmesIMDB$Bilheteria~as.factor(unlist(filmesIMDB$Awards)))

## correlação entre notas do IMDB e bilheteria
plot3 <- ggplot(filmesIMDB, aes(imdbRating, Bilheteria)) + geom_point()
plot3
plot3.genero.separado <- ggplot(filmesIMDB, aes(imdbRating, Bilheteria, col = as.factor(unlist(Genre)) )) + geom_point() + theme(legend.position = "none") + facet_wrap(~as.factor(unlist(Genre)), 5)
plot3.genero.separado
plot3.premios <- ggplot(filmesIMDB, aes(imdbRating, Bilheteria, col = as.factor(unlist(Awards)))) + geom_point()
plot3.premios
plot3.premios.separado <- plot3.premios + facet_grid(~as.factor(unlist(Awards)), 5)+ theme(legend.position = "none")
plot3.premios.separado

## correlação entre notas do Rotten e bilheteria 
plot4 <- ggplot(filmesIMDB, aes(Tomatoes, Bilheteria)) + geom_point()
plot4
plot4.genero.separado <- ggplot(filmesIMDB, aes(Tomatoes, Bilheteria, col = as.factor(unlist(Genre)))) + geom_point() + facet_wrap(~as.factor(unlist(Genre)), 5) + theme(legend.position = "none")
plot4.genero.separado
plot4.premios <- ggplot(filmesIMDB, aes(Tomatoes, Bilheteria, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot4.premios
plot4.premios.separado <- plot4.premios + facet_grid(~as.factor(unlist(Awards)),5) + theme(legend.position = "none")
plot4.premios.separado

## correlação entre orçamento e ano
plot5 <- ggplot(filmesIMDB,aes(Year,filmeBudget)) + geom_point()
plot5

## Overview
multiplot(plot1,plot5, plot2,plot3,plot4) #graficos gerais

