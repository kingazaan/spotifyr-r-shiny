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
library(reshape2)
library(plotly)
library(ggridges)


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
        as.data.frame(get_my_top_artists_or_tracks(type = 'artists', 
                                    time_range = 'long_term', 
                                    limit = 25) %>% 
                                    rename(followers = followers.total) %>% 
                                    select(.data$genres, .data$name, .data$popularity, .data$followers) %>% 
                                    rowwise %>% 
                                    mutate(genres = paste(.data$genres, collapse = ', ')) %>% 
                                    ungroup
        )
}

## datatableify fav_artists
fav_artists_datatable <- function() {
    datatable(fav_artists()) %>% formatStyle(c('name', 'genres', 'popularity', 'followers'), color = 'black')
}

# audio features for top artists table function
audio_features_fav_artist <- function(artist_name) {
    get_artist_audio_features(artist = artist_name, return_closest_artist = TRUE) %>% 
        select(.data$artist_name, .data$track_name, .data$album_name, .data$danceability, .data$energy, .data$loudness, .data$speechiness, .data$acousticness, .data$liveness, .data$valence, .data$tempo) %>% 
        distinct(.data$track_name, .keep_all= TRUE)
}


# ## favorite tracks table function
# fav_tracks <- function() {
#     ceiling(get_my_top_artists_or_tracks(type = 'tracks', include_meta_info = TRUE)[['total']] / 50) %>%
#         seq() %>%
#         map(function(x) {
#             get_my_top_artists_or_tracks(type = 'tracks', limit = 50, offset = (x - 1) * 50)
#         }) %>% reduce(rbind)
# }

## favorite artists join from previous function
# fav_tracks_artists <- function(prev) {
#     temp <-
#     prev %>%
#         select(artists) %>%
#         reduce(rbind) %>%
#         reduce(rbind) %>%
#         select(name)
#     
#     temp <-
#     temp %>%
#         select(name, album.name, popularity)
#     
#     prev <- temp %>%
#         full_join(prev, by = 'id') # %>%
#        # count(id, sort = TRUE) %>%
#        # unique() %>%
#        # select(-id) %>%
#        # top_n(20, n)
#     
#     prev <- prev %>%
#         full_join(temp, by = 'id') %>%
#         select(name, name.x, album.name.y, popularity.y)
#     
#     return(prev)
# }



# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Spotify User Analysis",
                    # Havbar 1, tabPanel
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
                    tabPanel("Popularity",
                             mainPanel(
                                 h3("Let's see how popular your favorite artists are on spotify"),
                                 plotOutput(outputId = "popularity_plot_output"),
                                 h3("We can also so how they compare to eachother, in terms of popularity"),
                                 plotlyOutput(outputId = "follower_plot_output"),
                                 br(), br(), br(), br(), br(), br(), br(), br(), 
                                 h3("Some more insight on your top artists:"),
                                 DT::dataTableOutput("favorite_artists_table"),
                                 # DT::dataTableOutput("favorite_tracks_table"),
                             ), # mainPanel
                             
                             
                    ), # Navbar 3, tabPanel
                    tabPanel("Sentiment",
                             sidebarPanel(
                                 selectInput("artist_name", "Choose one of your top artists: ", fav_artists()$name)
                             ), # sidebarPanel
                             mainPanel(
                                 h3("Let's take a look at the audio features for your most popular artists"),
                                 h2("Energy"),
                                 plotOutput(outputId = "energy_plot_output"),
                                 br(), br(),
                                 h2("Positivity (Valence)"),
                                 plotOutput(outputId = "valence_plot_output")
                             ), # mainPanel
                             
                             
                    ), # Navbar 4, tabPanel
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
        
#    output$favorite_tracks_table <- DT::renderDataTable({ datatable(fav_tracks_artists(fav_tracks()) %>% select(name, artists, album.name, popularity)) %>% formatStyle( c('name', 'album.name'), color = 'black') }) %>% bindEvent(validate)
    
    # TABPANEL #2
    
    output$popularity_plot_output <- renderPlot({
        data <- fav_artists()
        ggplot(data=data, aes(x=reorder(name, -popularity), y=popularity)) +
            geom_bar(stat="identity", width = .5, fill = "#1F51FF") +
            theme(axis.text.x = element_text(angle = 90)) + 
            xlab("Artist Name") + ylab("Popularity Score")
    })
    
    output$follower_plot_output  <- renderPlotly({
        data <- fav_artists()
        data$follow_percent <- data$followers / sum(data$followers)
        # ggplot(data=data, aes(x="", y=follow_percent, fill=name)) + 
        #     geom_bar(width = 1, stat = "identity") + 
        #     coord_polar("y", start=0)
        plot_ly(data=data, labels = ~name, values = ~follow_percent, type = 'pie', textposition = 'inside', textinfo = 'label+percent', width = 500, height = 550) %>%
            layout(autosize = T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$favorite_artists_table <- DT::renderDataTable({ fav_artists_datatable() }) %>% bindEvent(validate)
    
    # TABPANEL #3
    
    output$energy_plot_output <- renderPlot({ 
        data <- audio_features_fav_artist(as.character(input$artist_name)) %>% arrange(desc(energy))
        ggplot(data = data, aes(x = energy, y = fct_rev(album_name))) + 
            geom_density_ridges(scale = 2, quantile_lines=TRUE) +
            theme_ridges()+
            scale_x_continuous(expand = c(0, 0)) +
            labs(title= "Energy by album", subtitle = "Based on data from Spotify", y="Album", x = "Energy")
    })
    
    output$valence_plot_output <- renderPlot({ 
        data <- audio_features_fav_artist(as.character(input$artist_name)) %>% arrange(desc(valence))
        ggplot(data = data, aes(x = valence, y = fct_rev(album_name))) + 
            geom_density_ridges(scale = 2, size = 0.1, rel_min_height = 0.03, quantile_lines=TRUE, quantiles=2) +
            theme_ridges()+
            scale_x_continuous(expand = c(0, 0)) +
            labs(title= "Valence by album", subtitle = "Based on data from Spotify", y="Album", x = "Positivity")
    })
    
    
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)


