load("C:/Users/João Eduardo sola/Downloads/filmes2005")

#teste de correlação entre ano e bilheteria
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

library(ggplot2)
plot1 <- ggplot(filmesIMDB, aes(x = Year, y = filmeDomGross, col = as.factor(unlist(Genre)))) + geom_point()
plot1

#graficos para cada genero
plot2 <- plot1 + facet_wrap(~as.factor(unlist(Genre)), 5)
plot2

#calculando coeficiente de correlação (precisamos discutir isso, porque talvez não seja tão vantajoso esse cálculo sem tantas observações)
ano <- filmesIMDB$Year
bilheteria <- filmesIMDB$filmeDomGross
cor(ano,bilheteria, use = "complete.obs")

#Correlação entre Genero e Bilheteria
plot3 <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross, col = as.factor(Year))) + geom_point()
plor3
plot3.produção <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross)) + geom_point() + facet_wrap(~as.factor(Production),5)
plot3.produção
plot3.produção.cor <- ggplot(filmesIMDB, aes(filmeBudget, filmeDomGross, col = as.factor(Production))) + geom_point()
plot3.produção.cor #preciso de ajuda com esse

#Correlação entre notas do IMDB e Bilheteria
plot4 <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross)) + geom_point()
plot4
plot4.box <- ggplot(filmesIMDB, aes(imdbRating, filmeDomGross)) + geom_boxplot()
plot4.box
