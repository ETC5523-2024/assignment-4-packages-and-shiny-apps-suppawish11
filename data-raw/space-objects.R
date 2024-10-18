## code to prepare `space-objects` dataset goes here


url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv'

space_objects <- readr::read_csv(url) |>
  janitor::clean_names()



usethis::use_data(space_objects, overwrite = TRUE)
