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
library(httr)
library(remotes)

# Create functions to make plots

Sys.setenv(SPOTIFY_CLIENT_ID = "99c693033daf48b883ff86660b50d80a")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "9033b67165b24d63b641ac1c76dd7ea8")

access_token <- get_spotify_access_token()

## Authentification function
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
        rename(positivity = valence) %>% 
        select(.data$artist_name, .data$track_name, .data$album_name, .data$danceability, .data$energy, .data$loudness, .data$speechiness, .data$acousticness, .data$liveness, .data$positivity, .data$tempo) %>% 
        distinct(.data$track_name, .keep_all= TRUE)
}

## datatablify audio_features
sentiment_datatable <- function(artist_name) {
    datatable(audio_features_fav_artist(artist_name)) %>% formatStyle(c('artist_name', 'track_name', 'album_name', 'danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'liveness', 'positivity', 'tempo') ,color = 'black')
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
                    tabPanel("Intro",
                             sidebarPanel(
                                 tags$h3("Input:"),
                                 textInput("id", "Client ID:", ""), # txt1 sent to the server
                                 textInput("secret", "Client Secret:", ""),    # txt2 sent to the server
                                 actionButton("btn", "validate"),
                                 textOutput("validate_message")
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Welcome to the Spotify User Analysis tool!"),
                                 h6("Here you can see different analyses on your own music, as well as artists you follow, and what type of music you are interested in. Here are a couple first steps:"),
                                 br(),
                                 h6("Step 1: Login with your spotify account at https://developer.spotify.com/dashboard/"),
                                 h6("Step 2: Create an app, and copy the Client ID and Client Secret"),
                                 h6("Step 3: Enter them to the sidebar on the left and validate"),
                                 h6("Step 4: When prompted with the message are you ..., make sure to click NOT YOU and login yourself. Now you're good to go! "),
                                 # verbatimTextOutput("txtout"), # generated from the server
                             ) # mainPanel
                             
                             
                    ), # Navbar 2, tabPanel
                    tabPanel("Popularity",
                             mainPanel(
                                 h4("Let's see how popular your favorite artists are on spotify"),
                                 plotOutput(outputId = "popularity_plot_output"),
                                 h4("We can also see how they compare to eachother, in terms of amount of followers"),
                                 plotlyOutput(outputId = "follower_plot_output"),
                                 br(), br(), br(), br(), br(), br(), br(), br(), 
                                 h4("Some more insight on your top artists:"),
                                 DT::dataTableOutput("favorite_artists_table"),
                                 br(), br()
                                 # DT::dataTableOutput("favorite_tracks_table"),
                             ), # mainPanel
                             
                             
                    ), # Navbar 3, tabPanel
                    tabPanel("Sentiment",
                             # absolutePanel(
                             #     selectInput("artist_name", "Choose one of your top artists: ", fav_artists()$name)
                             # ), # sidebarPanel
                             mainPanel(
                                 fluidRow(
                                     column(width = 6,
                                            selectInput("artist_name", "Choose one of your top artists: ", fav_artists()$name),
                                     ),
                                     column(width = 6,
                                            selectInput("sentiment_type", "Choose one sentiment type: ", c('Danceability', 'Energy', 'Loudness', 'Speechiness', 'Acousticness', 'Liveness', 'Positivity', 'Tempo'))
                                     ),
                                 ),
                                 # absolutePanel(
                                 #     selectInput("artist_name", "Choose one of your top artists: ", fav_artists()$name),
                                 #     selectInput("sentiment_type", "Choose one sentiment type: ", c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'liveness', 'valence', 'tempo')),
                                 h4("Let's take a look at the audio features for your most popular artists"),
                                 tags$style("#sentiment_text
                                    {font-size: 30px;
                                    color: white;
                                    display: block;
                                    text-align: center;
                                    padding-bottom: 10px}"),
                                 textOutput("sentiment_text"),
                                 plotOutput(outputId = "sentiment_plot_output"),
                                 br(), br(),
                                 h6("The highest scoring song for this category is:"),
                                 tags$style("#most_sentiment
                                    {font-size: 20px;
                                    color: Chartreuse;
                                    display: block;
                                    text-align: left;
                                    padding-bottom: 10px}"),
                                 textOutput("most_sentiment"),
                                 numericInput("most", "", 1),
                                 h6("Use the dialog box above to see other songs and their scores!"),
                                 
                                 # h6("The lowest scoring song for this category is:"),
                                 # tags$style("#least_sentiment
                                 #    {font-size: 20px;
                                 #    color: Chartreuse;
                                 #    display: block;
                                 #    text-align: right;
                                 #    padding-bottom: 10px}"),
                                 # textOutput("least_sentiment"),
                                 
                                 # h3("Positivity (Valence)"),
                                 # plotOutput(outputId = "valence_plot_output"),
                                 # h5("The most positive song by this artist is:"),
                                 # textOutput("most_positive"),
                                 # h5("The least positive song by this artist is:"),
                                 # textOutput("least_positive"),
                                 # DT::dataTableOutput("sentiment_table"),
                                 
                                 br(), br()
                             ), # mainPanel
                             
                             
                    ), # Navbar 4, tabPanel
                    tabPanel("User Stats",
                             mainPanel(
                                 h4("Let's take a look at type of music you listen to overall, based on your top artists"),
                                 tags$style(
                                     "p { 
                                      color: red;
                                     }"
                                 ),
                                 p("Be patient, this could take a minute or two"),
                                 h3("Positivity vs Energy"),
                                 plotOutput("energy_vs_positivity_plot_output"),
                                 tags$style("#energy_vs_positivity
                                    {font-size: 40px;
                                    color: Yellow;
                                    display: block;
                                    text-align: center;
                                    padding-top: 25px;
                                    padding-bottom: 10px}"),
                                 textOutput("energy_vs_positivity")
                             ) 
                    ) 
                    
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
    popularity_data <- reactive({ fav_artists() })
    
    output$popularity_plot_output <- renderPlot({
        data <- popularity_data()
        ggplot(data=data, aes(x=reorder(name, -popularity), y=popularity)) +
            geom_bar(stat="identity", width = .5, fill = "#1F51FF") +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90)) + 
            xlab("Artist Name") + ylab("Popularity Score")
    })
    
    output$follower_plot_output <- renderPlotly({
        data <- popularity_data()
        data$follow_percent <- data$followers / sum(data$followers)
        # ggplot(data=data, aes(x="", y=follow_percent, fill=name)) + 
        #     geom_bar(width = 1, stat = "identity") + 
        #     coord_polar("y", start=0)
        plot_ly(data=data, labels = ~name, values = ~follow_percent, type = 'pie', textposition = 'inside', textinfo = 'label+percent', width = 550, height = 550) %>%
            layout(autosize = T,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$favorite_artists_table <- DT::renderDataTable({ fav_artists_datatable() }) %>% bindEvent(validate)
    
    # TABPANEL #3
    ## hella important, basically a global variable lowkey
    sentiment_data <- reactive({ audio_features_fav_artist(as.character(input$artist_name)) })
    sentiment_datatable_reactive <- reactive({ sentiment_datatable(input$artist_name) })
    
    output$sentiment_text <- renderText({ input$sentiment_type })
    
    output$sentiment_plot_output <- renderPlot({ 
        text1 <- text
        text <- casefold(input$sentiment_type, upper = FALSE)
        data <- sentiment_data() %>% arrange(desc(.data[[text]]))
        ggplot(data = data, aes(x = .data[[text]], y = fct_rev(album_name), fill = stat(x))) + 
            geom_density_ridges_gradient(scale = 2, quantile_lines=TRUE, quantiles = 2) +
            scale_fill_viridis_c(name = text, option = "D") + 
            theme_ridges(font_size = 12, center_axis_labels = TRUE) + 
            scale_x_continuous(expand = c(0.01, 0)) + 
            labs(y ="Album", x = .data[[text]])
    })
    
    output$most_sentiment <- renderText({
        text <- casefold(input$sentiment_type, upper = FALSE)
        data <- sentiment_data() %>% arrange(desc(.data[[text]]))
        paste(paste(data$track_name[input$most], " with a score of ", sep=""), data[[text]][input$most], sep="")
    })
    
    # output$least_sentiment <- renderText({
    #     text <- casefold(input$sentiment_type, upper = FALSE)
    #     data <- sentiment_data() %>% arrange(.data[[text]])
    #     paste(paste(data$track_name[1], " with a score of ", sep=""), data[[text]][1], sep="")
    # })
    
    # TABPANEL #3
    ## hella important, basically a global variable lowkey
    top_artist_sentiment_data <- reactive({
        names <- popularity_data()$name
        top_artist_sentiment <- as.data.frame(audio_features_fav_artist(names[1]))
        # 2:length(names) for all artists 
        for (i in 2:10) { 
            tryCatch(
                expr = {
                    top_artist_sentiment <- rbind(top_artist_sentiment, as.data.frame(audio_features_fav_artist(names[i])))
                },
                error = function(e){
                    print(e)
                }
            )
            # dynamicVariableName <- paste("fav_artist", i, sep="_")
        }
        return (top_artist_sentiment)
    })
    
    output$energy_vs_positivity_plot_output <- renderPlot({
        # PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
        ggplot(data = top_artist_sentiment_data(), aes(x = positivity, y = energy, color = artist_name)) +
            geom_jitter() +
            geom_vline(xintercept = 0.5) +
            geom_hline(yintercept = 0.5) +
            scale_x_continuous(limits = c(0, 1)) +
            scale_y_continuous(limits = c(0, 1)) +
            annotate('text', 0.25 / 2, 1, label = "Aggressive") +
            annotate('text', 1.75 / 2, 1, label = "Joyful") +
            annotate('text', 1.75 / 2, 0, label = "Chill") +
            annotate('text', 0.25 / 2, 0, label = "Sad") +
            labs(x= "Positivity", y= "Energy") +
            ggtitle("Emotional quadrant for Top 10 Artists")
    })
    
    output$energy_vs_positivity <- renderText({
        temp1 <- top_artist_sentiment_data()
        temp <- cbind(temp1$energy, temp1$positivity) 
        if(mean(temp[,1], na.rm = TRUE) < 0.500 & mean(temp[,2], na.rm = TRUE) < 0.500) {
            "You seem to enjoy sad music :,("
        } else if (mean(temp[,1], na.rm = TRUE) < 0.500 & mean(temp[,2], na.rm = TRUE) > 0.500) {
            "You seem to enjoy chill music :)"
        } else if (mean(temp[,1], na.rm = TRUE) > 0.500 & mean(temp[,2], na.rm = TRUE) < 0.500) {
            "You seem to enjoy aggressive music :("
        } else {
            "You seem to enjoy joyful music :D"
        }
    })
    
    # output$energy_plot_output <- renderPlot({ 
    #     data <- sentiment_data() %>% arrange(desc(energy))
    #     ggplot(data = data, aes(x = energy, y = fct_rev(album_name), fill = stat(x))) + 
    #         geom_density_ridges_gradient(scale = 2, quantile_lines=TRUE, quantiles = 2) +
    #         scale_fill_viridis_c(name = "Energy", option = "D") + 
    #         theme_ridges(font_size = 12, center_axis_labels = TRUE) +
    #         scale_x_continuous(expand = c(0.01, 0)) +
    #         labs(title= paste(input$artist_name ,"'s Energy by Album", sep=""), y ="Album", x = "Energy")
    # })
    # 
    # output$most_energetic <- renderText({
    #     data <- sentiment_data() %>% arrange(desc(energy))
    #     paste(paste(data$track_name[1], " with an energy score of ", sep=""), data$energy[1], sep="")
    # })
    # 
    # output$least_energetic <- renderText({
    #     data <- sentiment_data() %>% arrange(energy)
    #     paste(paste(data$track_name[1], " with an energy score of ", sep=""), data$energy[1], sep="")
    # })
    #     
    # output$valence_plot_output <- renderPlot({ 
    #     data <- sentiment_data() %>% arrange(desc(valence))
    #     ggplot(data = data, aes(x = valence, y = fct_rev(album_name), fill = stat(x))) + 
    #         geom_density_ridges_gradient(scale = 2, quantile_lines=TRUE, quantiles = 2) +
    #         scale_fill_viridis_c(name = "Positivity", option = "A") + 
    #         theme_ridges(font_size = 12, center_axis_labels = TRUE) +
    #         scale_x_continuous(expand = c(0.01, 0)) +
    #         labs(title= paste(input$artist_name ,"'s Positivity (Valence) by Album", sep=""), y="Album", x = "Positivity")
    # })
    # 
    # output$most_positive <- renderText({
    #     data <- sentiment_data() %>% arrange(desc(valence))
    #     paste(paste(data$track_name[1], " with an energy score of ", sep=""), data$valence[1], sep="")
    # })
    # 
    # output$least_positive <- renderText({
    #     data <- sentiment_data() %>% arrange(valence)
    #     paste(paste(data$track_name[1], " with an energy score of ", sep=""), data$valence[1], sep="")
    # })
    
    
    
    # output$sentiment_table <- DT::renderDataTable({ sentiment_datatable_reactive() }) %>% bindEvent(validate)
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)




