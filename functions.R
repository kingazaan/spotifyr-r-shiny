library(spotifyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(lubridate)

### Step 0: API authentification

id = "a9ddd67c426941c78b7744913d619b05"
secret = "6d82d70a9be9495f8c77ca1dc912c9b1"

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

access_token <- get_spotify_access_token()

### Step 1: Create input for user ID and SECRET

##############
###  TODO  ###
##############

# we will temporarily use my own id
my_id <- 'wmz6ywnpx3on6ygkxti3t3ay6'

### Step 2: Create function for viewing a user's favorite artists

favorite_artists <- get_my_top_artists_or_tracks(type = 'artists', 
                             time_range = 'short_term', 
                             limit = 20,
                             ) %>% 
#  select(name, followers.total, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup

# can chart these artists on a bar graph of popularity

view1 <- as.data.frame(view1)
view1 %>% ggplot(aes(x=reorder(name, desc(followers.total)), y=followers.total)) + geom_bar(stat='identity') + labs(x="", y="") + scale_y_continuous(labels=scales::comma) + ggtitle('Most listened to by Follower Count') + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)), axis.text.x=element_text(angle=50, size=10, vjust=0.5))


## get spotify id and look at personal playlists

### Step 3: Look at user's most favorite artists by number of songs

# create the favorite tracks file, and the audio features files
# favorite tracks
all_my_fav_tracks <-
  ceiling(get_my_top_artists_or_tracks(type = 'tracks', include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_top_artists_or_tracks(type = 'tracks', limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind) %>%  write_rds('raw_all_my_fav_tracks.rds')

view(all_my_fav_tracks)

artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)

track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)

# For numerical variables, sometimes for simplifying problems, cut them into fractions is a good idea. Here, we go further, we fill the column plot with different color to represent different frequency group.
track_num_artist %>%
  mutate(
    freq = case_when(
      n > 100 ~ 'More than 100 tracks',
      between(n, 50, 99) ~ '50~99 tracks',
      between(n, 20, 49) ~ '20~49 tracks',
      TRUE ~ 'Less than 20 tracks'
    )
  ) %>%
  # To avoid mess up the order of frequency group, I always suggest to convert the category variables as factor variables, with built-in order, levels.
  mutate(freq = factor(
    freq,
    levels = c(
      'More than 100 tracks',
      '50~99 tracks',
      '20~49 tracks',
      'Less than 20 tracks'
    )
  )) %>%
  ggplot(mapping = aes(
    x = reorder(name, -n),
    y = n,
    fill = freq
  )) +
  geom_col() +
  labs(fill = NULL,title = 'Who is My Favorite Artist',caption = 'data from spotify via spotiyr') +
  xlab('Artist') +
  ylab('Tracks Number') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -60),
        axis.title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        plot.caption = element_text(hjust = 1,face = 'bold.italic'))


### Step 4 find least popular tracks/artists

view(as.data.frame(read_rds('raw_all_my_fav_tracks.rds')))

track_bottom_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, -n)



