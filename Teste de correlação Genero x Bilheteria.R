load("C:/Users/João Eduardo sola/Downloads/filmes2005")

#teste de correlação entre genêro e bilheteria
genero <- list()
filmesIMDB$filmeDomGross <- as.numeric(gsub(",","",filmesIMDB$filmeDomGross))
filmesIMDB$Year <- as.numeric(filmesIMDB$Year)
for (i in 1:nrow(filmesIMDB)){
  a <- strsplit(filmesIMDB[i, 6], ",")
  genero <- append(genero, a[[1]][1])
}

genero

filmesIMDB$Genre <- genero

library(ggplot2)
ggplot(filmesIMDB, aes(x = Year, y = filmeDomGross, col = as.factor(unlist(Genre)))) + geom_point()
