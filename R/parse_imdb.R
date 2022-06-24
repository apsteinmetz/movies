# scratch pad

imdb_roach_raw <- read.delim("data/imdb roach.txt",header = FALSE)

regex = "^ ([0-9]{4})(.+) (\\(.+\\)) ?(\\(.+\\))?"
regex = "^ ([0-9]{4})(.+) ((\\(.+\\)) ?)"

imdb_roach <- imdb_roach_raw %>%
  extract(
    col = "V1",
    regex = regex,
    into = c("year", "title", "length", "role"),
    remove = TRUE
  ) %>%
  separate(col = "title",
           sep = "\\(",
           into = c("title", "length")) %>%
  mutate(across(
    .cols = everything(),
    .fns = function(x)
      str_remove(x, "\\(")
  )) %>%
  mutate(across(
    .cols = everything(),
    .fns = function(x)
      str_remove(x, "\\)")
  )) %>%
  mutate(across(
    .cols = everything(),
    .fns = trimws)
  ) %>%
  separate(col = "role",
           sep = " - ",
           into = c("role", "credited")) %>%
  mutate(length = if_else(is.na(length),"Feature",length)) %>%
  mutate(credited = if_else(is.na(credited),"credit",credited))



