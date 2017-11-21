# Trabalho ME115 - Shiny app

load("filmesPronto")

library(shiny)
library(plotly)
library(ggplot2)
library(shinyjs)
library(ggmap)

shinyUI(fluidPage(
  navbarPage(title=div(img(src="icons8-movie_projector.png"), "ME115 - Filmes"),
             tabPanel("Informações gerais",{
               sidebarLayout(
                 sidebarPanel(width = 12,
                              selectizeInput("select", label="", choices=c("", filmesIMDB$Title), multiple=FALSE, options = list(placeholder='Digite um filme'))
                              
                 ),
                 mainPanel(
                           splitLayout(cellWidths = c("50%", "100%"),
                                       uiOutput('poster'), align="center",
                                       tabsetPanel(id="tabsetpanel",
                                                   useShinyjs(),
                                                   tabPanel("Info",
                                                            fluidRow(column(12, h2(textOutput('title'))), align="left"),
                                                            fluidRow(column(12, h2("   "))),
                                                            fluidRow(column(1, uiOutput('calendar')), align="left",
                                                                     column(3, h4(uiOutput('releaseDate'))),
                                                                     column(1, uiOutput('time')),
                                                                     column(7, h4(textOutput('runtime')))),
                                                            fluidRow(column(1, uiOutput('money')), align="left", 
                                                                     column(3, h4(textOutput("budget"))),
                                                                     column(1, uiOutput('mood')),
                                                                     column(7, h4(textOutput('genre')))),
                                                            fluidRow(column(1, uiOutput('ticket')), align="left", 
                                                                     column(3, h4(textOutput("gross"))),
                                                                     column(1, uiOutput('trophy')),
                                                                     column(7, h4(textOutput('awards')))),
                                                            fluidRow(column(1, uiOutput('world')), align="left", 
                                                                     column(3, h4(textOutput("wwgross"))),
                                                                     column(1, uiOutput('direct')),
                                                                     column(7, h4(textOutput('director')))),
                                                            fluidRow(column(1, uiOutput('imdb')), align="left", 
                                                                     column(3, h4(textOutput("imdbrating"))),
                                                                     column(1, uiOutput('hand')),
                                                                     column(7, h4(textOutput('writer')))),
                                                            fluidRow(column(1, uiOutput('tomato')), align="left",
                                                                     column(3, h4(textOutput('tomatoes'))),
                                                                     column(1, uiOutput('star')),
                                                                     column(7, h4(textOutput('actors')))),
                                                            fluidRow(column(1, uiOutput('meta')), align="left",
                                                                     column(5, h4(textOutput('metacritic'))))
                                                   ),
                                                   tabPanel("Sinopse",
                                                            tags$br(),
                                                            tagAppendAttributes(textOutput("plot"), style="white-space:pre-wrap;font-size: 16px")
                                                   ),
                                                   tabPanel("Trailer", id="trail",
                                                            tags$br(),
                                                            uiOutput('trailer'))
                                       ))
                           
                 )
               )
             }),
             navbarMenu("Gráficos",
                        tabPanel("Plot Maker",
                                 sidebarPanel(width=3,
                                              sliderInput("year", "Ano de lançamento", 1915, 2018, value = c(1915, 2018)),
                                              selectInput("genre", "Gênero (um filme pode ter mais de um gênero)",
                                                          c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
                                                            "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                                                            "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                                            "Short", "Sport", "Thriller", "War", "Western")),
                                              checkboxInput("grossDom", "Bilheteria USA", TRUE),
                                              checkboxInput("grossWW", "Bilheteria Mundial", FALSE),
                                              selectInput("xvar", "Eixo X", c("Custo ($)", "Nota imdB", "Nota Rotten Tomatoes", "Nota Metacritic", "Ano de lançamento"))
                                              ),
                                 mainPanel(plotlyOutput('grafico2'))
                                 
                                 ),
                        tabPanel("Mapa coroplético",
                                 plotlyOutput("Map",width="100%",height="800px")
                                 )),
             tabPanel("Sobre",{
               mainPanel(splitLayout(cellWidths = c("50%", "50%"),
                 {tabPanel("Sobre1",
                           h4("Feito com"),
                           tags$br(),
                           uiOutput('rstudio'),
                           tags$br(),
                           h4("APIs utilizada"),
                           tags$br(),
                           fluidRow(column(7, uiOutput('omdb')),
                                    column(5,uiOutput('tmdb'))),
                           h4("Fontes"),
                           tags$br(),
                           fluidRow(column(4, uiOutput('imdbfonte')), align="center",
                                    column(3, uiOutput('metacriticfonte')),
                                    column(4, uiOutput('rottenfonte')))
                 )
                   
                 },
                 {tabPanel("Sobre2",
                           h4("Estruturação de Dados"),
                           tags$br(),
                           fluidRow(column(3, uiOutput('devtools')),
                                    column(3, uiOutput('dplyr')),
                                    column(3, uiOutput('magrittr'))),
                           tags$br(),
                           h4("Aplicação"),
                           tags$br(),
                           fluidRow(column(3, uiOutput('shiny')),
                                    column(3, uiOutput('ggplot2')),
                                    column(3, uiOutput('plotly'))),
                           tags$br(),
                           h4("Grupo"),
                           h6("     "),
                           h5("Juliana Daikawa"), 
                           h5("João Eduardo Sola"),
                           h5("Iago Campos"),
                           h5("Ignácio Amstalden")
                           )
                   
                 }
               )
                 
                          
               )
             }))

)
)