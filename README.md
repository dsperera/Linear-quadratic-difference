
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Linear.quadratic.difference

<!-- badges: start -->
<!-- badges: end -->

This function calculates linear and quadratic fits for a given two-
variable data set, and then calculates the difference between the fit
curves at a specified x value. The difference is calculated as
“quadratic” - “linear”. This allows the two types of fits to be easily
and conveniently compared for a given data set.

## Installation

This package is not yet on CRAN. It can be downloaded as follows:

``` r
devtools::install_github("dsperera/Linear.quadratic.difference", ref = "0.2.0")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Linear.quadratic.difference)

# Specify a data set with two or more variables

tib2 <- tibble::tribble(
  ~ a, ~ b, ~ c,
  -7, 7, 1,
  -5, 6.4, 2,
  -3, 4.1, 3,
  -1, 4, 4,
  1, 2.3, 5,
  3, 1, 6,
  5, -0.7, 7
)

# Specify an x value of interest at which to calculate the difference

xoi2 = -2

# Call the function, selecting the required columns of the data set

lin_quad_diff(tib2$a, tib2$b, xoi2)
#> [1] 0.1651786
```
