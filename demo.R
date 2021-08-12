library(spotifyr)
library(tidyverse)
library(devtools)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggridges)

## used https://github.com/charlie86/spotifyr to aid in constructing the functions

## authenticate the spotify client stuff
id = "a9ddd67c426941c78b7744913d619b05"
secret = "6d82d70a9be9495f8c77ca1dc912c9b1"

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

access_token <- get_spotify_access_token()

## get spotify id and look at personal playlists
my_id <- 'wmz6ywnpx3on6ygkxti3t3ay6'
my_playlists <- get_user_playlists(my_id)

## sample
beatles <- get_artist_audio_features('the beatles')

# not working currently
# tracks <- get_playlist_tracks(my_plists)
# features <- get_track_audio_features(tracks)

## find recently played music
get_my_recently_played(limit = 15) %>% 
  mutate(
    artist.name = map_chr(track.artists, function(x) x$name[1]),
    played_at = as_datetime(played_at)
  ) %>% 
  select(
    all_of(c("track.name", "artist.name", "track.album.name", "played_at"))
  ) %>% 
  kable()

## Find Your All Time Favorite Artists
get_my_top_artists_or_tracks(type = 'artists', 
                             time_range = 'long_term', 
                             limit = 5) %>% 
  select(.data$name, .data$genres) %>% 
  rowwise %>% 
  mutate(genres = paste(.data$genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

## Find your favorite tracks at the moment
get_my_top_artists_or_tracks(type = 'tracks', 
                             time_range = 'short_term', 
                             limit = 5) %>% 
  mutate(
    artist.name = map_chr(artists, function(x) x$name[1])
  ) %>%  
  select(name, artist.name, album.name) %>% 
  kable()

## can measure musical joyfulness using valenceand get_artist_audio_features

## analyzing my "NB" playlist, seeing which track best represents me

my_playlists2 <- my_playlists %>%
  filter(name %in% c('Arab'))
tracks <- get_playlist_tracks(my_playlists2$id)
features <- get_track_audio_features(tracks$track.id)
tracks2 <- tracks %>%
  left_join(features, by=c('track.id' = 'id'))

tolerance = 0.05
tracks2 %>%
  filter((energy>mean(energy)-tolerance)&(energy<mean(energy)+tolerance)&
           (valence>mean(valence)-tolerance)&(valence<mean(valence)+tolerance)&
           (track.popularity/100>mean(track.popularity/100)-tolerance)&(track.popularity/100<mean(track.popularity/100)+tolerance)) %>%
  mutate(artist.name = map_chr(track.album.artists, function(x) x$name[1])) %>% 
  select(artist.name,track.name,energy,valence,track.popularity) %>%
  bind_rows(data.frame(artist.name = '---average---',track.name = '---average---',energy=mean(tracks2$energy),valence=mean(tracks2$valence),track.popularity=mean(tracks2$track.popularity))) %>%
  arrange(energy) %>% kable() %>% 
  kable_styling("striped", full_width = F, position = "left")%>% 
  row_spec(row = 2, color = "black",bold = TRUE)

## Find insights on an artist's music

foo <- get_artist_audio_features('foo fighters')
nrow(foo)

foo <- foo %>% 
  select(track_name, album_name, valence, danceability,energy,loudness,speechiness,liveness,tempo,album_release_year,track_number) %>% 
  arrange(album_release_year) %>%
  unite(album,album_name,album_release_year,sep=' - ',remove=FALSE)
foo$album_name <- as.factor(foo$album_name)
foo$album_release_year <- as.factor(foo$album_release_year)

ggplot(foo, aes(x = energy,y=fct_rev(album_release_year))) + 
  geom_density_ridges(scale = 2, size = 0.1, rel_min_height = 0.03,quantile_lines=TRUE,quantiles=2) +
  theme_ridges()+
  scale_y_discrete(labels=c('Concrete and Gold - 2017','Sonic Highways - 2014','Wasting Light - 2011','Echoes, Silence, Patience & Grace - 2007','In Your Honor - 2005','One By One - 2002', 'There Is Nothing Left To Lose - 1999','The Colour And The Shape - 1997','Foo Fighters - 1995'))+ 
  scale_x_continuous( expand = c(0, 0)) +
  labs(title= "Foo Fighters energy by album",subtitle = "Based on data from Spotify", y="Album", x = "Energy")

## Find songs with most energy from an artist

foo %>% 
  arrange(-energy) %>% 
  select(track_name, album, energy) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)


## Analyze a specific album

foo %>% 
  arrange(track_number) %>% 
  filter(album_name == 'The Colour And The Shape') %>% 
  select(track_name,track_number,energy,valence,tempo) %>% 
  mutate(
    energy = cell_spec(energy, "html", bold = ifelse(energy == max(energy), TRUE, FALSE)),
    valence = cell_spec(valence, "html", bold = ifelse(valence == max(valence), TRUE, FALSE))
  ) %>%
  kable(format = 'html',escape=F) %>% 
  kable_styling("striped", full_width = F, position = "left",bootstrap_options = c("striped", "hover", "condensed", "responsive"))



