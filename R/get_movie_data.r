# get pre-code actors and movies
library(tidyverse)
library(TMDb)
# need to downgrade support package for extrafont
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# library(extrafont)
library(sysfonts)
library(showtext)
library(ggimage)
library(magick)

# proprietary to my system
api_key <- Sys.getenv("TMDB_V3_API_KEY")


sysfonts::font_add_google("Poiret One")
sysfonts::font_add_google("Righteous")
sysfonts::font_add_google("Limelight")
showtext::showtext_auto()

if (!file.exists("data/genres.rdata")) {
  genres <- genres_movie_list(api_key)$genres %>%
    as_tibble() %>%
    bind_rows(tibble(id = 0, name = "Unknown")) %>%
    mutate(across(.fns = as.factor)) %>%
    rename(genre_id = id, genre = name) %>%
    {.}
  save(genres, file = "data/genres.rdata")
}
min_runtime = 59


LOCALES = factor(c("English","NonEnglish","Global"))
LOCALE = "Global"
# --------------------------------------------------
# Get all movies for a year
pre_code_years <- as.character(1929:1934)

# get first page and find out how many pages
get_page_count <- function(year){
  discover_movie(api_key = api_key,
                 primary_release_year = year,
                 page=1)$total_pages
}

get_movie_page_for_year <- function(page_num,year){
  print(paste(year,page_num))
  movie_page <- discover_movie(api_key = api_key,
                               primary_release_year = year,
                               page=page_num)$result %>%
    as_tibble()
  return(movie_page)
}

full_year <- function(target_year){
  page_count <- get_page_count(target_year)
  result <- 1:page_count %>%
  map(get_movie_page_for_year,target_year) %>%
  bind_rows() %>%
  mutate(release_year = target_year,.after="release_date")
  return(result)
}

if (!file.exists("data/pre_code_movies_raw.rdata")){
  pre_code_movies_raw <- pre_code_years %>%
    map(full_year) %>%
    bind_rows()

  save(pre_code_movies_raw,file="data/pre_code_movies_raw.rdata")
}
# get supplemental data including revenue, budget, running time
# and imdb_id
movie2 <- function(movie_id){
  # needs error checking
  print(movie_id)
  return(movie(api_key,movie_id))
}

if (!file.exists("data/pre_code_movies_sup_raw.rdata")){
  pre_code_movies_sup_raw <- pre_code_movies_raw$id %>%
  map(movie2)
save(pre_code_movies_sup_raw,file = "data/pre_code_movies_sup_raw.rdata")
}

# --------------------------------------------------
# get cast and crew as list columns
get_credits <- function(movie_id) {
  print(movie_id)
  credits <- movie_credits(api_key = api_key, id = movie_id) %>%
    enframe() %>%
    pivot_wider() %>%
    unnest("id") %>%
    rename(movie_id = id)
  if (length(credits$cast[[1]] > 0) &
      length(credits$crew[[1]] > 0)) {
    credits$cast[[1]] <- credits$cast[[1]] %>%
      as_tibble() %>%
      rename(person_id = id) %>%
      mutate(gender=as_factor(gender)) %>%
      select(-profile_path,-credit_id,-adult)
    credits$crew[[1]] <- credits$crew[[1]] %>%
      as_tibble() %>%
      rename(person_id = id) %>%
      mutate(gender=as_factor(gender)) %>%
      select(-profile_path,-credit_id,-adult)
    return(credits)
    # if no cast or crew
  } else return(NULL)
}

if (!file.exists("data/pre_code_credits.rdata")){
  pre_code_credits <- as.character(pre_code_movies$movie_id) %>%
    map(get_credits) %>%
    bind_rows() %>%
    mutate(movie_id %>% as_factor(movie_id))

  save(pre_code_credits,file="data/pre_code_credits.rdata")

}
# -----------------------------------------------
# data wrangling
# gender assumption from 1905 SS data based on 60% minimum
# gender majority
load("~/R Projects/movies/data/genders_1905.rdata")
# allow for trans since TMDB does
levels(genders_1905$gender) <- c("F","M","U","T")
load(file="data/pre_code_credits.rdata")
load(file="data/pre_code_movies_raw.rdata")
load(file="data/pre_code_movies_sup_raw.rdata")
load(file="data/genres.rdata")
iso_langauges <- read_csv("data/iso_langauges.csv",show_col_types = FALSE) %>%
  filter(iso_2_letter != "") %>%
  transmute(original_language = iso_2_letter,language_name = english_name) %>%
  unique()

pre_code_movies <- pre_code_movies_raw %>%
  select(release_year,
         release_date,
         title,
         id,
         genre_ids,
         original_language,
         original_title,
         popularity,
         vote_count,
         vote_average,
         overview
  ) %>%
  rename(movie_id = id) %>%
  mutate(movie_id = as_factor(movie_id)) %>%
  mutate(original_language =as.factor(original_language)) %>%
  mutate(english =(original_language == "en")) %>%
  mutate(language = as_factor(if_else(english,"English","NonEnglish"))) %>%
  mutate(release_year = as.numeric(release_year)) %>%
  mutate(release_date = as.Date(release_date)) %>%
  left_join(iso_langauges,by="original_language")

# add supplemental data
pre_code_movies_sup <- lapply(pre_code_movies_sup_raw,function(x){
  data.frame(movie_id = x[['id']],
             budget = x[['budget']],
             revenue =x[['revenue']],
             imdb_id = ifelse(is.null(x[['imdb_id']]),NA,x[['imdb_id']]),
             runtime =x[['runtime']])
}) %>%
  bind_rows() %>%
  mutate(movie_id = as.factor(movie_id))

pre_code_movies <- pre_code_movies %>%
  left_join(pre_code_movies_sup,by="movie_id")

# more clean up
# fix empty genre_id rows
for (n in 1:nrow(pre_code_movies)){
  if(length(pre_code_movies$genre_ids[[n]])==0) pre_code_movies$genre_ids[[n]] <- 0
}

# Filter function for language
movies_by_language <- function(language){
  switch(EXPR=language,
         English = filter(pre_code_movies,original_language == "en"),
         NonEnglish = filter(pre_code_movies,original_language != "en"),
         Global = pre_code_movies)
}

pre_code_cast <- pre_code_credits %>%
  unnest(cols = "cast") %>%
  select(-crew,-known_for_department) %>%
  mutate(person_id = as_factor(person_id)) %>%
  mutate(movie_id = as_factor(movie_id)) %>%
  rename(person_popularity = popularity) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = fct_recode(gender,F= '1',M = '2',U = '0',T = "3")) %>%
  # guess unknown genders
  mutate(first = word(name)) %>%
  left_join(genders_1905,by="first") %>%
  mutate(gender = if_else(gender.x == "U" & !is.na(gender.y),
                          gender.y,gender.x),
         .before = "gender.x") %>%
  select(-gender.x,-gender.y,-first) %>%
  # limit to actors in English-language films
  inner_join(movies_by_language(LOCALE)) %>%
  filter(runtime > min_runtime)

# scale billing order to count
# appearance of top-billed actor
# more than a supporting actor
wgt_order <- tibble(order = 0:100,
                    billing_factor  = c(1,1,1,0.5,0.5,0.25,0.25,rep(0.1,94)))

ranked_cast <- pre_code_cast %>%
  #  inner_join(pre_code_movies) %>%
  left_join(wgt_order, by = "order") %>%
  group_by(person_id) %>%
 summarise(
   name,
   gender,
   appearances = n(),
   person_popularity = mean(person_popularity),
   avg_billing = mean(order),
   avg_billing_factor = mean(billing_factor)
 ) %>%
 unique() %>%
  mutate(bankability = appearances * avg_billing_factor,.before = "avg_billing") %>%
 arrange(desc(appearances)) %>%
  {.}

# CREW
pre_code_crew <- pre_code_credits %>%
  unnest(cols = "crew") %>%
  select(-cast,-known_for_department) %>%
  mutate(person_id = as_factor(person_id)) %>%
  mutate(movie_id = as_factor(movie_id)) %>%
  rename(person_popularity = popularity) %>%
  mutate(gender = fct_recode(gender,F= '1',M = '2',U = '0')) %>%
  mutate(gender = fct_expand(gender,"T")) %>%
  # guess unknown genders
  mutate(first = word(name)) %>%
  left_join(genders_1905,by="first") %>%
  mutate(gender = if_else(gender.x == "U" & !is.na(gender.y),
                          gender.y,gender.x),
         .before = "gender.x") %>%
  select(-gender.x,-gender.y,-first) %>%
  # limit to English-language films
  inner_join(movies_by_language(LOCALE)) %>%
  filter(runtime > min_runtime) %>%
  {.}

directors <- pre_code_crew %>%
  filter(job == "Director") %>%
  count(name) %>%
  arrange(desc(n))

# genres
movies_by_genre <- movies_by_language(LOCALE) %>%
  select(movie_id,title,genre_ids,runtime,english,language,vote_average,vote_count) %>%
  unnest(genre_ids) %>%
   mutate(genre_id = as_factor(genre_ids)) %>%
   mutate(short = (runtime < 60)) %>%
   left_join(genres,by="genre_id") %>%
   select(movie_id,title,genre,short,english,language,vote_average,vote_count) %>%
   filter(!str_detect(genre,"TV")) %>%
  {.}


ranked_genres <- movies_by_genre %>%
  count(genre) %>% arrange(n) %>%
  mutate(genre = as_factor(as.character(genre)))


movies_by_genre$genre <- fct_relevel(movies_by_genre$genre,levels(ranked_genres$genre))

animated_movies <- movies_by_genre %>%
  filter(genre == "Animation") %>%
  transmute(movie_id,animated = TRUE)

movies_by_genre <- movies_by_genre %>%
  left_join(animated_movies) %>%
  replace_na(list(animated=FALSE)) %>%
  filter(genre != "Animation")

# --------------------------------------------------
# PLOTS
text_color = "gold"


# top appearances
p <- ranked_cast %>%
  ungroup() %>%
  filter(gender == "F") %>%
  slice_max(order_by = appearances, n=20) %>%
  mutate(name = fct_rev(as_factor(name))) %>%
  ggplot(aes(name,appearances)) + geom_col(fill = "gold") +
  coord_flip() +
  labs(title = '"Hardest Working" Pre-Code Actresses',
       y = "Number of Roles (1929-1934)",
       x = "",
       caption = "Source: themoviedb.org") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm"))
ggimage::ggbackground(p, "img/deco background.jpg")

p <- pre_code_crew %>%
  filter(job == "Producer") %>%
  count(name) %>%
  ungroup() %>%
  slice_max(order_by = n, n=20) %>%
  mutate(name = fct_rev(as_factor(name))) %>%
  ggplot(aes(name,n)) + geom_col(fill = "gold") +
  coord_flip() +
  labs(title = "Most Prolific Pre-Code Producers",
       y = "Number of Films (1929-1934)",
       x = "",
       caption = "Source: themoviedb.org") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm"))
ggimage::ggbackground(p, "img/deco background.jpg")

#bankability
p <- ranked_cast %>%
  ungroup() %>%
  filter(gender == "F") %>%
  slice_max(order_by = bankability, n=20) %>%
  mutate(name = fct_rev(as_factor(name))) %>%
  ggplot(aes(name,bankability)) + geom_col(fill = "gold") +
  coord_flip() +
  labs(title = 'Most "Bankable" Pre-Code Actoresses',
       y = "Appearances and Billing (1929-1934)",
       x = "",
       caption = "Source: themoviedb.org") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm"))
ggimage::ggbackground(p, "img/deco background.jpg")

p <- ranked_cast %>%
  #  filter(bankability > 20,person_popularity > 2) %>%
  ggplot(aes(person_popularity,bankability)) + geom_point(color = text_color) +
  geom_text(aes(label=ifelse(bankability>55,name,'')),hjust=-.1,vjust=0,color = text_color) +
  geom_text(aes(label=ifelse(person_popularity>16,name,'')),hjust=.7,vjust=-0.6,color = text_color) +
  labs(title = 'Popularity Today vs. "Bankability" Then',
       y = "Bankability (Appearances and Billing)",
       x = "Relative Hits at TMDB",
       caption = "Source: themoviedb.org, Art Steinmetz") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm"))
ggimage::ggbackground(p, "img/deco background.jpg")

# Genres
p <- movies_by_genre %>%
  filter(short == FALSE) %>%
  arrange(n) %>%
  group_by(genre,language) %>%
  summarise(n = n()) %>%
  mutate(genre = fct_reorder(genre,n)) %>%
  ggplot(aes(genre,n,fill = language)) + geom_col() +
  labs(title = 'Pre-Code Feature Genres',
       subtitle = "(Genres Not Mutually Exclusive)",
       y = "Count",
       x = "",
       caption = "Source: themoviedb.org") +
  scale_fill_discrete(type = c("brown","gold")) +
  theme(text = element_text(family = "Limelight",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Righteous",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm")) +
  coord_flip()
ggimage::ggbackground(p, "img/deco background.jpg")

# live-action shorts
p <- movies_by_genre %>%
  filter(short == TRUE) %>%
  filter(animated == FALSE) %>%
  count(genre) %>%
  arrange(n) %>%
  mutate(genre = as_factor(as.character(genre))) %>%
  ggplot(aes(genre,n)) + geom_col(fill = text_color) +
  labs(title = 'Pre-Code Live-Action Shorts Genres',
       subtitle = "(Genres Not Mutually Exclusive)",
       y = "Count",
       x = "Genre",
       caption = "Source: themoviedb.org") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm")) +
  coord_flip()
ggimage::ggbackground(p, "img/deco background.jpg")

# animated shorts
p <- movies_by_genre %>%
  filter(short == TRUE) %>%
  filter(animated == TRUE) %>%
  count(genre) %>%
  arrange(n) %>%
  mutate(genre = as_factor(as.character(genre))) %>%
  ggplot(aes(genre,n)) + geom_col(fill = text_color) +
  labs(title = 'Pre-Code Animated Shorts Genres',
       subtitle = "(Genres Not Mutually Exclusive)",
       y = "Count",
       x = "Genre",
       caption = "Source: themoviedb.org") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm")) +
  coord_flip()
ggimage::ggbackground(p, "img/deco background.jpg")

# languages
p <- movies_by_language("NonEnglish") %>%
  filter(runtime > min_runtime) %>%
  count(language_name) %>%
  arrange(n) %>%
  slice_max(order_by=n,n=10) %>%
  mutate(language_name = fct_reorder(language_name,n)) %>%
  ggplot(aes(language_name,n)) + geom_col(fill = text_color) +
  labs(title = 'Top 10 Feature Languages\nin the Pre-Code Era',
       subtitle = "(English excluded)",
       y = "Count",
       x = "",
       caption = "Source: themoviedb.org") +
  scale_y_continuous(breaks = seq(0,500,by = 100)) +

  theme(text = element_text(family = "Limelight",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Righteous",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm")) +
  coord_flip()
ggimage::ggbackground(p, "img/deco background.jpg")

#popularity today
p <- ranked_cast %>%
  #  filter(bankability > 20,person_popularity > 2) %>%
  ggplot(aes(person_popularity,bankability)) + geom_point(color = text_color) +
  geom_text(aes(label=ifelse(bankability>55,name,'')),hjust=-.1,vjust=0,color = text_color) +
  geom_text(aes(label=ifelse(person_popularity>16,name,'')),hjust=.7,vjust=-0.6,color = text_color) +
  labs(title = 'Popularity Today vs. "Bankability" Then',
       y = "Bankability (Appearances and Billing)",
       x = "Relative Hits at TMDB",
       caption = "Source: themoviedb.org, Art Steinmetz") +

  theme(text = element_text(family = "Poiret One",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Poiret One",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2,.8,2,.8, "cm"))
ggimage::ggbackground(p, "img/deco background.jpg")


movies_by_language("English") %>%
  filter(vote_count > 5) %>%
  pull(vote_average) %>%
  mean()

# Distribution of Ratings
p <- movies_by_language("English") %>%
  filter(vote_count > 5) %>%
  filter(vote_average > 0) %>%
  ggplot(aes(vote_average)) +
  geom_histogram(fill = text_color,color="black", binwidth = 0.5) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = 'Pre-Code Movie Ratings at TMDB',
       subtitle = "Films With More Than Five Raters",
       y = "Number of Movies",
       x = "Average Rating",
       caption = "Source: themoviedb.org"
  ) +

  theme(text = element_text(
    family = "Poiret One",
    color = text_color,
    size = 20
  )) +
  theme(text = element_text(family = "Limelight",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Righteous",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2, .8, 2, .8, "cm"))

ggimage::ggbackground(p, "img/deco background.jpg")


# Top-Rated Movies
temp <- movies_by_language("English") %>%
  filter(vote_count > 50) %>%
  filter(vote_average > 0) %>%
  slice_max(order_by = vote_average,n=10) %>%
  unnest(genre_ids) %>%
  mutate(genre_ids = as_factor(genre_ids)) %>%
  rename(genre_id = genre_ids) %>%
  left_join(genres) %>%
  select(movie_id,title,genre,vote_average,vote_count
         ) %>%
  group_by(movie_id,title,vote_average,vote_count) %>%
  summarise(genres = toString(genre)
            ) %>%
  left_join(filter(pre_code_cast,order<2)) %>%
  group_by(title,vote_average,vote_count,genres) %>%
  summarise(Stars = toString(name)
  ) %>%
  arrange(desc(vote_average)) %>%
  write_csv(file="data/top_films.csv") %>%
  {.}

p <- movies_by_language("English") %>%
  filter(vote_count > 5) %>%
  filter(vote_average > 0) %>%
  ggplot(aes(vote_average)) +
  geom_histogram(fill = text_color,color="black", binwidth = 0.5) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = 'Pre-Code Movie Ratings at TMDB',
       subtitle = "Films With More Than Five Raters",
       y = "Number of Movies",
       x = "Average Rating",
       caption = "Source: themoviedb.org"
  ) +
  theme(text = element_text(family = "Limelight",color = text_color,size = 20)) +
  theme(axis.text = element_text(family = "Righteous",color = text_color)) +
  theme(axis.line = element_line(color = text_color)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.margin = margin(2, .8, 2, .8, "cm"))

ggimage::ggbackground(p, "img/deco background.jpg")

