
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spaceobs

<!-- badges: start -->
<!-- badges: end -->

The goal of spaceobs is to provide an analysis of the data set of yearly
number of objects launched into outer space through shiny app build
inside this package.

## Installation

You can install the development version of spaceobs like so:

``` r
# install.packages("remotes")
remotes::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-suppawish11")
```

## Example

``` r
library(spaceobs)
```

#### Dataset

``` r
space_objects
```

To view the yearly number of objects launched into outer space dataset.

#### Entity list

``` r
entity_list() |> 
  head()
#> [1] "APSCO"     "Algeria"   "Angola"    "Arabsat"   "Argentina" "Armenia"
```

To retrieve the entity names that are in the `space_objects` dataset.

#### Year range of selected entity

``` r
year_entity("United States")
#> [1] 1958 2023
```

To retrieve the year range of the selected entity in the `space_objects`
dataset.

#### Launching Shiny App

``` r
run_app()
```
