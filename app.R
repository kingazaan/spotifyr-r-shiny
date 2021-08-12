#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# install.packages('shiny')
# install.packages('shinythemes')
# 
# Sample client id and server:
# id = "a9ddd67c426941c78b7744913d619b05"
# secret = "6d82d70a9be9495f8c77ca1dc912c9b1"

# Load R packages
library(shiny)
library(shinythemes)
library(spotifyr)
library(tidyverse)
library(DT)


# Create functions to make plots

## Authentification funtion
authenticate <- function(id, secret) {
    # authenticate the spotify client stuff
    Sys.setenv(SPOTIFY_CLIENT_ID = id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
    
    access_token <- get_spotify_access_token()
}

## favorite artists table function
fav_artists <- function() {
    get_my_top_artists_or_tracks(type = 'artists', 
                                time_range = 'long_term', 
                                limit = 25) %>% 
#                                select(.data$name, .data$genres) %>% 
                                rowwise %>% 
                                mutate(genres = paste(.data$genres, collapse = ', ')) %>% 
                                ungroup
}

## favorite tracks table function
fav_tracks <- function() {
    ceiling(get_my_top_artists_or_tracks(type = 'tracks', include_meta_info = TRUE)[['total']] / 50) %>%
        seq() %>%
        map(function(x) {
            get_my_top_artists_or_tracks(type = 'tracks', limit = 50, offset = (x - 1) * 50)
        }) %>% reduce(rbind)
}

## favorite artists join from previous function
fav_tracks_artists <- function(prev) {
    view(temp) <-
    prev %>%
        select(artists) %>%
        reduce(rbind) %>%
        reduce(rbind) %>%
        select(name, popularity)
    
    temp <-
    temp %>%
        select(name, album.name, popularity)
    
    prev <- temp %>%
        full_join(prev, by = 'id') # %>%
       # count(id, sort = TRUE) %>%
       # unique() %>%
       # select(-id) %>%
       # top_n(20, n)
    
    prev <- prev %>%
        full_join(temp, by = 'id') %>%
        select(name, name.x, album.name.y, popularity.y)
    
    return(prev)
}



# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Spotify User Analysis",
                    tabPanel("User Stats",
                             sidebarPanel(
                                 tags$h3("Input:"),
                                 textInput("id", "Client ID:", ""), # txt1 sent to the server
                                 textInput("secret", "Client Secret:", ""),    # txt2 sent to the server
                                 actionButton("btn", "validate"),
                                 textOutput("validate_message")

                             ), # sidebarPanel
                             mainPanel(
                                 h1("Header 1"),
                                 h4("Output 1"),
                                 verbatimTextOutput("txtout"), # generated from the server

                             ) # mainPanel
                             
                    ), # Navbar 2, tabPanel
                    tabPanel("Artist Stats",
                             mainPanel(
                                 h2("These are your artist stats:"),
                                 DT::dataTableOutput("favorite_artists_table"),
                                 h2("These are your favorite tracks:"),
                                 DT::dataTableOutput("favorite_tracks_table"),
                                 plotOutput(outputId = "tempPlotOutput")
                             ), # mainPanel

                    ), # Navbar 3, tabPanel
                    tabPanel("Recommendations", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    
    validate <- eventReactive(input$btn, {authenticate(input$id, input$secret)})
    output$validate_message <- renderText({ validate() })

    # output$txtout <- renderText({
    #     paste( input$id, input$secret, sep = " " ) # this is where the input + output happens
    # })
        
    output$favorite_artists_table <- DT::renderDataTable({ datatable(fav_artists()) %>% formatStyle( c('name', 'genres'), color = 'black') }) %>% bindEvent(validate)
    
    output$favorite_tracks_table <- DT::renderDataTable({ datatable(fav_tracks_artists(fav_tracks()) %>% select(name, artists, album.name)) %>% formatStyle( c('name', 'album.name'), color = 'black') }) %>% bindEvent(validate)
    
    output$tempPlotOutput <- renderPlot({
        x <- airquality$Wind
        x <- na.omit(x)
        bins <- seq(min(x), max(x), length.out = 10)
        
        hist(x, breaks = bins, col = "red", border = "black",
             xlab = "Wind level",
             main = "This is a placeholder")
    })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)