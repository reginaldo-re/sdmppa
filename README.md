
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdmppa

<!-- badges: start -->
<!-- badges: end -->

The package generates pseudo-absence points (PAP) to be used in species
distribution modeling (SDM) and environmental niche modeling (ENM). The
process involves taking an environmental space represented by a set of
points P and a set of presence points (PP) as input and then generating
points that mimic environmental conditions of a true absence. The
resulting PPA is a subset of P and does not contain any points in common
with PP, meaning that PP and PPA are disjoint sets. The package provides
several different methods of PPA generation.

## Installation

You can install the development version of sdmppa like so:

``` r
devtools::install_github("reginaldo-re/sdmppa")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sdmppa)
ES <- dplyr::tibble(
  cell_id = c(
    75000, 81017, 81374, 83506, 83508,
    84360, 84783, 84785, 86047, 86468
  ),
  bio_1 = c(
    25.24835, 26.47237, 25.09730, 25.28250, 25.25749,
    25.43329, 25.33722, 25.36212, 25.50991, 25.84756
  ),
  bio_2 = c(
    12.365706, 10.976032, 11.488524, 11.492427, 11.356132,
    11.477320, 11.125202, 11.271169, 10.878525, 10.985706
  ),
  bio_3 = c(
    68.53027, 66.84972, 70.87223, 70.63369, 70.99750,
    70.81410, 70.95460, 71.04424, 71.87370, 72.48579
  ),
  bio_4 = c(
    48.37795, 54.44825, 42.10769, 42.21368, 40.86964,
    41.39428, 41.72614, 41.17366, 44.82830, 46.00434
  )
)
sp1 <- dplyr::tibble(
  cell_id = c(75000, 81017, 84360, 86468)
)
PPA <- ES |> ppa_random(sp1, ES_idx = "cell_id")
#> ℹ Joining ES and P by cell_id.
```
