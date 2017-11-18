load("C:/Trabalhos Unicamp/ME115 Teste/filmes_1_ate_5180")

filmesIMDB$filmeDomGross <- as.numeric(gsub(",","",filmesIMDB$filmeDomGross))/1000000 ## Ajustando para numérico em milhoes

filmesIMDB <- subset(filmesIMDB, is.na(filmesIMDB$imdbVotes) == 'FALSE') ## Retirando o valor nulo



media_votos <- mean(filmesIMDB$imdbVotes)

votos_maior <- subset(filmesIMDB, filmesIMDB$imdbVotes >= media_votos )



plot <- votos_maior[,c(17,31)]


plot <- na.omit(plot) 

quais <- unique(plot$filmeDomGross)


corr <- cor(x = plot$imdbRating, y = plot$filmeDomGross)
library(ggplot2)



ggplot(plot, aes(x = imdbRating, y = filmeDomGross, colour = imdbRating) ) + geom_point() + geom_smooth(method = "lm", se = FALSE, col = 'indianred1')  + ggtitle("Bilheteria x Nota") +xlab("Nota") + ylab("Bilheteria em milhões") + theme(panel.background = element_rect(fill = 'seashell2', colour = 'darkgoldenrod1')) ## Ajustar cores para cada intervalo







