
load("filmesPronto")
load("paisesGravados")

library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(ggmap)
library(maps)
library(countrycode)

load("paisesGravados")
c$c <- as.character(c$c)
c$CODE <- NA
c <- c[-45,]

for (i in 1:nrow(c)) {
  c$CODE[i] <- countrycode(c$c[i], 'country.name', 'iso3c')
}
  
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

mapGravacoes <- plot_geo(c) %>%
  add_trace(
    z = ~Count, color = ~Count, colors = 'Purples',
    text = ~c, locations = ~CODE, marker = list(line = l)
  ) %>%
  layout(
    title = 'Gravacoes pelo Mundo',
    geo = g
  )

for (i in 1:nrow(filmesIMDB)) {
  if (grepl("win", filmesIMDB$Awards[i]) || grepl("Won", filmesIMDB$Awards[i])) {
    filmesIMDB$Win[i] <- 1
  } else {
    filmesIMDB$Win[i] <- 0
  }
}

shinyServer(
  
  function(input, output, session) {
    
    observeEvent(input$select,{
      if(input$select=="") {
        hide(id="tabsetpanel", anim = TRUE)
      } else {
        updateTabsetPanel(session, "tabsetpanel", selected = "Info")
        show(id="tabsetpanel", anim = TRUE)
      }
    })

    output$title <- renderText({
      if(is.null(input$select) || input$select=="") {
        return()
      } else {
        paste0(as.character(input$select))
      }
    })
    output$year <- renderText({
      if(is.null(input$select) || input$select=="") {
        return()
      } else {
        paste0(" (", as.character(filmesIMDB$Year[which(filmesIMDB$Title==as.character(input$select))[1]]), ")")
      }
    })
    output$poster <- renderUI({
      if(is.null(input$select) || input$select=="") {
        return()
      } else {
        if (filmesIMDB$Poster[which(filmesIMDB$Title==as.character(input$select))]=="N/A") {
          img(src="image-not-available.jpg", width=300)
        } else {
          img(src=filmesIMDB$Poster[which(filmesIMDB$Title==as.character(input$select))], width=300)
        }
      }
    })
    output$calendar <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-calendar.png")
      }
    })
    output$money <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-usd.png")
      }
      })
    output$ticket <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-train_ticket.png")
      }
    })
    output$world <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-globe.png")
      }
    })
    output$ploticon <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-hand_with_pen.png")
      }
    })
    output$imdb <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-imdb.png")
      }
    })
    output$releaseDate <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$Released[which(filmesIMDB$Title==as.character(input$select))])) {
          a <- as.character(filmesIMDB$Released[which(filmesIMDB$Title==as.character(input$select))[1]])
          format(as.Date(a), "%d %B %Y")
        } else {
          "-"
        }
      }
    })
    output$tomato <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if (!is.na(filmesIMDB$Tomatoes[which(filmesIMDB$Title==as.character(input$select))])) {
          if (as.numeric(gsub("%", "", filmesIMDB$Tomatoes[which(filmesIMDB$Title==as.character(input$select))])) >= 60) {
            img(src="icons8-tomato.png")
          } else {
            img(src="icons8-rotten_tomatoes.png")
          }
        } else {
          img(src="icons8-tomato.png")
        }
      }
    })
    output$meta <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="metacritic.png", width=50)
      }
    })
    output$time <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-historical.png", width=45)
      }
    })
    output$mood <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-theatre_mask.png")
      }
    })
    output$trophy <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-trophy.png", width=45)
      }
    })
    output$direct <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-clapperboard.png", width=45)
      }
    })
    output$hand <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-hand_with_pen.png", width=45)
      }
    })
    output$star <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        img(src="icons8-filled_star.png")
      }
    })
    output$rstudio <- renderUI({
      img(src="RStudio.png", width=300)
    })
    output$omdb <- renderUI({
      img(src="omdbapi.png",width=200)
    })
    output$tmdb <- renderUI({
      img(src="TMDB.png", width=100)
    })
    output$devtools <- renderUI({
      img(src="devtools.png", width=70)
    })
    output$dplyr <- renderUI({
      img(src='dplyr.png', width=70)
    })
    output$magrittr <- renderUI({
      img(src='magrittr.png', width=70)
    })
    output$shiny <- renderUI({
      img(src="shiny.png", width=70)
    })
    output$ggplot2 <- renderUI({
      img(src="ggplot2.png", width=70)
    })
    output$plotly <- renderUI({
      img(src="plotly.png", width=70)
    })
    output$imdbfonte <- renderUI({
      img(src="imdb.png", width=90)
    })
    output$rottenfonte <- renderUI({
      img(src="rottentomatoes.png", width=140)
    })
    output$metacriticfonte <- renderUI({
      img(src="metacriticicon.png", width=90)
    })
    output$budget <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$filmeBudget[which(filmesIMDB$Title==as.character(input$select))])) {
          paste0("$",as.character(filmesIMDB$filmeBudget[which(filmesIMDB$Title==as.character(input$select))[1]]))
        } else {
          "-"
        }
      }
    })
    output$gross <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$filmeDomGross[which(filmesIMDB$Title==as.character(input$select))])) {
          paste0("$",as.character(filmesIMDB$filmeDomGross[which(filmesIMDB$Title==as.character(input$select))[1]]))
        } else {
          "-"
        }
      }
    })
    output$wwgross <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$filmeWWGross[which(filmesIMDB$Title==as.character(input$select))])) {
          paste0("$",as.character(filmesIMDB$filmeWWGross[which(filmesIMDB$Title==as.character(input$select))[1]]))
        } else {
          "-"
        }
      }
    })
    output$plot <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if (!filmesIMDB$Plot[which(filmesIMDB$Title==as.character(input$select))]=="N/A") {
          filmesIMDB$Plot[which(filmesIMDB$Title==as.character(input$select))]
        } else {
          "-"
        }
      }
    })
    output$imdbrating <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$imdbRating[which(filmesIMDB$Title==as.character(input$select))])) {
          paste0(as.character(filmesIMDB$imdbRating[which(filmesIMDB$Title==as.character(input$select))[1]]), "/10")
        } else {
          "-"
        }
      }
    })
    output$tomatoes <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$Tomatoes[which(filmesIMDB$Title==as.character(input$select))])) {
          as.character(filmesIMDB$Tomatoes[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$metacritic <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!filmesIMDB$Metascore[which(filmesIMDB$Title==as.character(input$select))] == "N/A") {
          paste0(as.character(filmesIMDB$Metascore[which(filmesIMDB$Title==as.character(input$select))[1]]),"/100")
        } else {
          "-"
        }
      }
    })
    output$runtime <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$Runtime[which(filmesIMDB$Title==as.character(input$select))])) {
          as.character(filmesIMDB$Runtime[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$genre <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        as.character(filmesIMDB$Genre[which(filmesIMDB$Title==as.character(input$select))[1]])
      }
    })
    output$awards <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!filmesIMDB$Awards[which(filmesIMDB$Title==as.character(input$select))] == "N/A") {
          as.character(filmesIMDB$Awards[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$director <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!filmesIMDB$Director[which(filmesIMDB$Title==as.character(input$select))] == "N/A") {
          as.character(filmesIMDB$Director[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$writer <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!filmesIMDB$Writer[which(filmesIMDB$Title==as.character(input$select))] == "N/A") {
          as.character(filmesIMDB$Writer[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$actors <- renderText({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!filmesIMDB$Actors[which(filmesIMDB$Title==as.character(input$select))] == "N/A") {
          as.character(filmesIMDB$Actors[which(filmesIMDB$Title==as.character(input$select))[1]])
        } else {
          "-"
        }
      }
    })
    output$trailer <- renderUI({
      if (is.null(input$select) || input$select=="") {
        return()
      } else {
        if(!is.na(filmesIMDB$trailerKeys[which(filmesIMDB$Title==as.character(input$select))])) {
          trailerYT <- paste0("//www.youtube.com/embed/", filmesIMDB$trailerKeys[which(filmesIMDB$Title==as.character(input$select))])
          tags$iframe(src=trailerYT, width = 600, height = 400)
        } else {
          "-"
        }
      }
    })
    
    output$grafico2 <- renderPlotly({
      min <- input$year[1]
      max <- input$year[2]
      if (input$genre == as.character("All")) {
        graf <- subset(subset(filmesIMDB, as.numeric(substring(filmesIMDB$Released, 1, 4))>=min),as.numeric(substring(Released, 1, 4))<=max)
      } else {
        grafgenre <- subset(filmesIMDB, grepl(as.character(input$genre),filmesIMDB$Genre))
        graf <- subset(subset(grafgenre, as.numeric(substring(filmesIMDB$Released, 1, 4))>=min),as.numeric(substring(Released, 1, 4))<=max)
      }
      if (input$grossDom) {
        Gross <- as.numeric(gsub(",","",graf$filmeDomGross))
      }
      if (input$grossWW) {
        Gross <- as.numeric(gsub(",","",graf$filmeWWGross))
      }
      if (input$grossDom & input$grossWW) {
        Gross <- as.numeric(gsub(",","",graf$filmeDomGross))+as.numeric(gsub(",","",graf$filmeWWGross))
      }
      if (input$xvar == "Custo ($)") {
        g <- ggplot(graf, aes(x=as.numeric(gsub(",","",filmeBudget)),y=Gross, color=as.factor(Win))) + geom_point(alpha=0.7) + xlab("Custo ($)") + ylab("Bilheteria ($)") +labs(color="Prêmio")
      }
      if (input$xvar == "Nota imdB") {
        g <- ggplot(graf, aes(x=imdbRating,y=Gross, color=as.factor(Win))) + geom_point(alpha=0.7) + xlab("Nota (IMDb)") + ylab("Bilheteria ($)") +labs(color="Prêmio")
      }
      if (input$xvar == "Nota Rotten Tomatoes") {
        g <- ggplot(graf, aes(x=as.numeric(gsub("%","",Tomatoes)),y=Gross, color=as.factor(Win))) + geom_point(alpha=0.7) + xlab("Nota (Rotten Tomatoes)") + ylab("Bilheteria ($)") +labs(color="Prêmio")
      }
      if (input$xvar == "Nota Metacritic") {
        g <- ggplot(graf, aes(x=as.numeric(gsub("N/A","NA",Metascore)),y=Gross, color=as.factor(Win))) + geom_point(alpha=0.7) + xlab("Nota (Metacritic)") + ylab("Bilheteria ($)") +labs(color="Prêmio")
      }
      if (input$xvar == "Ano de lançamento") {
        g <- ggplot(graf, aes(x=as.numeric(substring(Released, 1, 4)),y=Gross, color=as.factor(Win))) + geom_point(alpha=0.7) + xlab("Ano de lançamento") + ylab("Bilheteria ($)") +labs(color="Prêmio")
      }
      ggplotly(g)
    })
    
    output$graficoteste <- renderPlotly({
      p <- ggplot(filmesIMDB, aes(x=as.numeric(gsub(",","",filmeBudget)),y=as.numeric(gsub(",","",filmeWWGross)), color=as.factor(Genre))) + geom_point() + theme(legend.position = 'none')
      ggplotly(p)
    })
    
    output$Map <- renderPlotly({
      mapGravacoes
    })

  }
)
