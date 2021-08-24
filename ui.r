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

Sys.setenv(SPOTIFY_CLIENT_ID = "a9ddd67c426941c78b7744913d619b05")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "42e668de12ca4b1abd81cee80f060846")

access_token <- get_spotify_access_token()

# Define UI
shinyUI(fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Spotify User Analysis",
                  # Havbar 1, tabPanel
                  tabPanel("Intro",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("id", "Client ID: ", ""), # txt1 sent to the server
                             textInput("secret", "Client Secret: ", ""),    # txt2 sent to the server
                             actionButton("btn", "validate"),
                             textOutput("validate_message")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Welcome to the Spotify User Analysis tool!"),
                             h6("Here you can see different analyses on your own music, as well as artists you follow, and what type of music you are interested in. Here are a couple first steps:"),
                             br(),
                             h6("Step 1: Go to https://developer.spotify.com/dashboard/ and login with your Spotify information"),
                             h6("Step 2: Create an app with name and description temp, then find the client ID and Client Secret"),
                             h6("Step 3: Copy and paste the ID and Secret into the designated dialog boxes, and click validate."),
                             h6("Step 4: Allow spotify to authenticate your account"),
                             h6("Now you should be good to go! Click one of the tabs above and learn more about your music"),
                             # h6("Step 4: When prompted with the message are you ..., make sure to click NOT YOU and login yourself. Now you're good to go! "),
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
                             textOutput("energy_vs_positivity"),
                             h3("Speechiness vs Danceability"),
                             plotOutput("speechiness_vs_danceability_plot_output"),
                             tags$style("#speechiness_vs_danceability
                                    {font-size: 40px;
                                    color: Yellow;
                                    display: block;
                                    text-align: center;
                                    padding-top: 25px;
                                    padding-bottom: 10px}"), 
                             textOutput("speechiness_vs_danceability")
                           ) 
                  ) 
                  
                ) # navbarPage
) # fluidPage
)