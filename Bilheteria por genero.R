load("C:/Trabalhos Unicamp/ME115 Teste/filmes_1_ate_5180")

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







action <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Action"))
adventure <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Adventure"))
animation <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Animation"))
biography <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Biography"))
comedy <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Comedy"))
crime <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Crime"))
documentary <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Documentary"))
drama <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Drama"))
horror <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Horror"))
romance <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Romance"))
scifi <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Sci-Fi"))
family <- na.omit(subset(filmesIMDB[,c(6,32)], filmesIMDB$Genre == "Family"))


generos <- rbind(action,adventure,animation,biography, comedy, crime, documentary, drama, family, horror, romance, scifi)


plot1.genero <- ggplot(generos, aes(x = Bilheteria, col = as.factor(unlist(Genre)))) + geom_histogram(breaks = seq(0,250, by = 20)) + facet_wrap(~as.factor(unlist(Genre)), 3) + theme(legend.position = "none") 
plot1.genero


a <- mean(action$Bilheteria)
b <- mean(adventure$Bilheteria)
c <- mean(animation$Bilheteria)
d <-mean(biography$Bilheteria)
e <-mean(comedy$Bilheteria)
f <-mean(crime$Bilheteria)
g <-mean(documentary$Bilheteria)
h <-mean(drama$Bilheteria)
i <-mean(family$Bilheteria)
j <-mean(horror$Bilheteria)
k <-mean(romance$Bilheteria)
l <-mean(scifi$Bilheteria)

generos <- as.data.frame(matrix(nrow = 12, ncol = 2))
generos[1:12,1] <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Horror", "Romance", "Sci-Fi")

generos[1:12,2] <- c(a,b,c,d,e,f,g,h,i,j,k,l)

ggplot(generos, aes(x = generos$V1, y = generos$V2)) + geom_point(colour = "darkslategray"  ) + xlab("Gêneros") + ylab("Bilheteria em milhões") + theme(panel.background = element_rect(fill = 'seashell2', colour = 'darkgoldenrod1')) + ggtitle("Média de bilheteria em Milhões por gênero ")
