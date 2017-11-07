load("filmes_1_ate_5180")
library(ggplot2)

#funçao para usarmos um genero
genero <- list()
filmesIMDB$filmeDomGross <- as.numeric(gsub(",","",filmesIMDB$filmeDomGross))
filmesIMDB$filmeBudget <- as.numeric(gsub(",","",filmesIMDB$filmeBudget))
filmesIMDB$Year <- as.numeric(filmesIMDB$Year)
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
####################################################################################################################
# testes envolvendo bilheteria
## correlação entre ano e bilheteria
plot1 <- ggplot(filmesIMDB, aes(x = Year, y = filmeDomGross, col = as.factor(unlist(Genre)), label=sprintf("%0.2f", round(filmeDomGross, digits = 2)))) + geom_point()
plot1
plot1.genero <- plot1 + facet_wrap(~as.factor(unlist(Genre)), 5)
plot1.genero
cor(filmesIMDB$Year, filmesIMDB$filmeDomGross, use = "complete.obs")

## correlação entre orçamento e bilheteria
plot2 <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross)) + geom_point()
plot2 + geom_smooth()
plot2.premios <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot2.premios
plot2.premios2 <- plot2.premios + facet_wrap(~as.factor(unlist(Awards)), 5)
plot2.premios2
plot2.bp.premios <- boxplot(filmesIMDB$filmeDomGross~as.factor(unlist(filmesIMDB$Awards)))
cor(filmesIMDB$filmeBudget, filmesIMDB$filmeDomGross, use = "complete.obs")

## correlação entre notas do IMDB e bilheteria
plot3 <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross)) + geom_point()
plot3
plot3.genero <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross, col = as.factor(unlist(Genre)) )) + geom_point()
plot3.genero
plot3.genero.separado <- plot3.genero + facet_wrap(~as.factor(unlist(Genre)), 5)
plot3.genero.separado
plot3.premios <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot3.premios
plot3.premios.separado <- plot3.premios + facet_wrap(~as.factor(unlist(Awards)), 5)
plot3.premios.separado
cor(filmesIMDB$imdbRating, filmesIMDB$filmeDomGross, use = "complete.obs")

## correlação entre notas do Rotten e bilheteria 
plot4 <- ggplot(filmesIMDB, aes(Tomatoes, filmeDomGross)) + geom_point()
plot4
multiplot(plot3,plot4)
plot4.genero <- ggplot(filmesIMDB, aes(Tomatoes, filmeDomGross, col = as.factor(unlist(Genre)) )) + geom_point()
plot4.genero
plot4.genero.separado <- plot4.genero + facet_wrap(~as.factor(unlist(Genre)), 5)
plot4.genero.separado
plot4.premios <- ggplot(filmesIMDB, aes(Tomatoes, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot4.premios
plot4.premios.separado <- plot4.premios + facet_wrap(~as.factor(unlist(Awards)),5)
plot4.premios.separado
multiplot(plot3.premios,plot4.premios)
cor(filmesIMDB$Tomatoes, filmesIMDB$filmeDomGross, use = "complete.obs")

##modelo de regressão para tentar predizer a bilheteria de um filme 
reg.bil <- lm(filmesIMDB$filmeDomGross~filmesIMDB$Tomatoes + filmesIMDB$imdbRating + filmesIMDB$filmeBudget + filmesIMDB$Year)
reg.bil
summary(reg.bil)
step(reg.bil, data=na.omit(filmesIMDB), direction="backward",trace=TRUE)

 ###################################################################################################################
# testes envolvendo notas IMDB
## correlação entre premios e notas do IMDB
plot4 <- ggplot(filmesIMDB, aes(as.factor(unlist(Awards)),imdbRating)) + geom_boxplot() + facet_wrap(~as.factor(unlist(Awards)), 5) #precisa juntar os graficos 
plot4
plot4.orcamento <- ggplot(filmesIMDB, aes(filmeBudget, imdbRating)) + geom_point()
plot4.orcamento
plot4.orcamento.genero <- ggplot(filmesIMDB, aes(filmeBudget, imdbRating, col = as.factor(unlist(Genre)))) + geom_point()
plot4.orcamento.genero
plot4.orcamento.genero.separado <- plot4.orcamento.genero + facet_wrap(~as.factor(unlist(Genre)),5)
plot4.orcamento.genero.separado
plot4.orcamento.premio <- ggplot(filmesIMDB, aes(filmeBudget, imdbRating, col = as.factor(unlist(Awards)))) + geom_point()
plot4.orcamento.premio
plot4.orcamento.premio.separado <- plot4.orcamento.premio + facet_grid(~as.factor(unlist(Awards)),5)
plot4.orcamento.premio.separado


plotpremio.rotten <- ggplot(filmesIMDB, aes(as.factor(unlist(Awards)), Tomatoes)) + geom_point()
plotpremio.rotten
