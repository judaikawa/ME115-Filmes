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

####################################################################################################################
# testes envolvendo bilheteria
## correlação entre ano e bilheteria
plot1 <- ggplot(filmesIMDB, aes(x = Year, y = filmeDomGross, col = as.factor(unlist(Genre)))) + geom_point()
plot1
plot1.genero <- plot1 + facet_wrap(~as.factor(unlist(Genre)), 5)
plot1.genero

## correlação entre orçamento e bilheteria
plot2 <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross)) + geom_point()
plot2 + geom_smooth()
plot2.premios <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot2.premios
plot2.premios2 <- plot2.premios + facet_wrap(~as.factor(unlist(Awards)), 5)
plot2.premios2
plot2.bp.premios <- ggplot(filmesIMDB, aes(as.factor(unlist(Awards)),filmeDomGross)) + geom_boxplot() + facet_wrap(~as.factor(unlist(Awards)), 5)
plot2.bp.premios

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




########################################  João, nao quero cagar o que tu já fez, entao to separando s2 bjo na bunda  ############################################################################
# testes envolvendo bilheteria
## correlação entre ano e bilheteria
plot1 <- ggplot(filmesIMDB, aes(x = Year, y = filmeDomGross, col = as.factor(unlist(Genre)))) + geom_point()
plot1

plot1.genero <- plot1 + facet_wrap(~as.factor(unlist(Genre)), 5)
plot1.genero + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre ano e bilheteria") + labs(y = "Bilheteria", x = "Ano")

## correlação entre orçamento e bilheteria

plot2 <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross)) + geom_point()
plot2 + geom_smooth() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre orçamento e bilheteria") + labs(y = "Bilheteria", x = "Orçamento")

plot2.premios <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot2.premios + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre orçamento e bilheteria") + labs(y = "Bilheteria", x = "Orçamento")

plot2.premios2 <- plot2.premios + facet_wrap(~as.factor(unlist(Awards)), 5)
plot2.premios2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre orçamento e bilheteria") + labs(y = "Bilheteria", x = "Orçamento")

plot2.bp.premios <- ggplot(filmesIMDB, aes(as.factor(unlist(Awards)),filmeDomGross)) + geom_boxplot() + facet_wrap(~as.factor(unlist(Awards)), 5)
plot2.bp.premios + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre orçamento e bilheteria") + labs(y = "Bilheteria", x = "Orçamento")

## correlação entre notas do IMDB e bilheteria
plot3 <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross)) + geom_point()
plot3 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre notas do IMDB e bilheteria") + labs(y = "Bilheteria", x = "Nota IMDB")

plot3.genero <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross, col = as.factor(unlist(Genre)) )) + geom_point()
plot3.genero + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre notas do IMDB e bilheteria") + labs(y = "Bilheteria", x = "Nota IMDB")

plot3.genero.separado <- plot3.genero + facet_wrap(~as.factor(unlist(Genre)), 5)
plot3.genero.separado + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre notas do IMDB e bilheteria") + labs(y = "Bilheteria", x = "Nota IMDB")

plot3.premios <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross, col = as.factor(unlist(Awards)))) + geom_point() #premios
plot3.premios + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre notas do IMDB e bilheteria") + labs(y = "Bilheteria", x = "Nota IMDB")

plot3.premios.separado <- plot3.premios + facet_wrap(~as.factor(unlist(Awards)), 5)
plot3.premios.separado + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Correlação entre notas do IMDB e bilheteria") + labs(y = "Bilheteria", x = "Nota IMDB")

###################################################################################################################
