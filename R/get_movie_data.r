# get pre-code actors and movies
library(tidyverse)
library(TMDb)


api_key <- Sys.getenv("TMDB_V3_API_KEY")


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

pre_code_movies <- pre_code_years %>%
  map(full_year) %>%
  bind_rows()


