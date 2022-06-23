# get pre-code actors and movies
library(tidyverse)
library(TMDb)

# proprietary to my system
api_key <- Sys.getenv("TMDB_V3_API_KEY")

genres <- genres_movie_list(api_key)$genres %>%
  as_tibble() %>%
  mutate(across(.fns = as.factor)) %>%
  rename(genre_id = id)

# --------------------------------------------------
# Get all movies for a year
pre_code_years <- as.character(1929:1934)
test_year = "1930"

# get first page and find out how many pages
get_page_count <- function(year){
  discover_movie(api_key = api_key,
                 primary_release_year = year,
                 page=1)$total_pages
}

get_movie_page_for_year <- function(page_num,year){
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

pre_code_movies_raw <- pre_code_years %>%
  map(full_year) %>%
  bind_rows()

save(pre_code_movies_raw,file="data/pre_code_movies_raw.rdata")
# can't save nested genre lists
#write_csv(pre_code_movies_raw,file="data/pre_code_movies_raw.csv")


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


pre_code_credits <- pre_code_movies_raw$id %>%
  map(get_credits) %>%
  bind_rows()

save(pre_code_credits,file="data/pre_code_credits.rdata")

# -----------------------------------------------
# data wrangling
# gender assumption from 1905 SS data based on 60% minimum
# gender majority
load("~/R Projects/movies/data/genders_1905.rdata")
# allow for trans since TMDB does
levels(genders_1905$gender) <- c("F","M","U","T")

load(file="data/pre_code_credits.rdata")
load(file="data/pre_code_movies_raw.rdata")

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
  mutate(release_year = as.numeric(release_year)) %>%
  mutate(release_date = as.Date(release_date)) %>%
  mutate(original_language =as.factor(original_language)) %>%
  filter(original_language == "en")


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
  inner_join(pre_code_movies)


ranked_cast <- pre_code_cast %>%
#  inner_join(pre_code_movies) %>%
  group_by(person_id) %>%
  mutate(avg_billing = mean(order)) %>%
  summarise(appearances = n(),name,gender,person_popularity,avg_billing) %>%
  unique() %>%
  arrange(desc(appearances)) %>%
  {.}

ranked_cast$bankability <- (10*(1-percent_rank(ranked_cast$avg_billing))) *
  ranked_cast$appearances

temp <- ranked_cast %>% filter(bankability > 25)


actor <- "Louise Brooks"
temp <- pre_code_cast %>%
  filter(name == actor) %>%
  left_join(pre_code_movies) %>%
  arrange(release_date)

temp <- tibble(order = sample(ranked_cast$avg_billing,10)) %>%
  arrange(order) %>%
  mutate(pct = 1-percent_rank(order)) %>%
  mutate(wgt_order = log(pct*10))
plot(temp$order,temp$wgt_order)

# CREW
pre_code_crew <- pre_code_credits %>%
  unnest(cols = "crew") %>%
  select(-cast,-known_for_department) %>%
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
  # limit to English-language films
  inner_join(pre_code_movies) %>%
  {.}

directors <- pre_code_crew %>%
  filter(job == "Director") %>%
  count(name) %>%
  arrange(desc(n))
