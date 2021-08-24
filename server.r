# server size
options(shiny.maxRequestSize=30*1024^2)

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

shinyServer(function(input, output, session) {
  
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
    names <- rev(popularity_data()$name)
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
  
  output$speechiness_vs_danceability_plot_output <- renderPlot({
    # PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
    ggplot(data = top_artist_sentiment_data(), aes(x = acousticness, y = danceability, color = artist_name)) +
      geom_jitter() +
      geom_vline(xintercept = 0.5) +
      geom_hline(yintercept = 0.5) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      annotate('text', 0.25 / 2, 1, label = "Hip-Hop/Club") +
      annotate('text', 1.75 / 2, 1, label = "Rock/Metal") +
      annotate('text', 1.75 / 2, 0, label = "Lyrical Rap") +
      annotate('text', 0.25 / 2, 0, label = "Smooth Instrument-Based") +
      labs(x= "acousticness", y= "danceability") +
      ggtitle("Music Genre for Top 10 Artists")
  })
  
  output$speechiness_vs_danceability <- renderText({
    temp1 <- top_artist_sentiment_data()
    temp <- cbind(temp1$acousticness, temp1$danceability) 
    if(mean(temp[,1], na.rm = TRUE) < 0.500 & mean(temp[,2], na.rm = TRUE) < 0.500) {
      "You seem to enjoy more singing-type music such as jazz or classical, not upbeat"
    } else if (mean(temp[,1], na.rm = TRUE) > 0.500 & mean(temp[,2], na.rm = TRUE) < 0.500) {
      "You seem to enjoy faster, more wordy music such as rap music or spoken word music with minimal instruments"
    } else if (mean(temp[,1], na.rm = TRUE) < 0.500 & mean(temp[,2], na.rm = TRUE) > 0.500) {
      "You seem to enjoy music you can vibe to, with an upbeat temp and more singing"
    } else {
      "You seem to enjoy faster-paced Rap music or very lyrical rock music"
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
  
}) # server