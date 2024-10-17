## code to prepare `space-objects` dataset goes here

library(tidyverse)
library(janitor)

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv'

space_objects <- read_csv(url) |>
  clean_names()



usethis::use_data(space_objects, overwrite = TRUE)
