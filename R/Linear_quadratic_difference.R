#' @title Difference between linear and quadratic fits at a given x value
#'
#' @description This function calculates linear and quadratic fits for a given two-
#' variable data set, and then calculates the difference between the fit curves at
#' a specified x value. The difference is calculated as "quadratic" - "linear".
#'
#' @param x A vector with the x-values of the data set to be fitted. Entries must
#' be in the same order as the y vector.
#'
#' @param y A vector with the y-values of the data set to be fitted. Entries must
#' be in the same order as the x vector.
#'
#' @param x_of_int "x of interest". The x value at which the difference between
#' the fits is to be calculated.
#'
#' @return The difference between the quadratic and linear fit curves at the given
#' x_of_int value. The difference is calculated as "quadratic" minus "linear".
#'
#' @examples
#' tib1 <- tibble::tribble(
#'  ~ one, ~ two,
#'  1, .8,
#'  2, 4.1,
#'  3, 8.8,
#'  4, 16.3,
#'  5, 24.7
#'  )
#' lin_quad_diff(tib1$one, tib1$two, 2)
#'
#' tib2 <- tibble::tribble(
#'  ~ a, ~ b, ~ c,
#'  -7, 7, 1,
#'  -5, 6.4, 2,
#'  -3, 4.1, 3,
#'  -1, 4, 4,
#'  1, 2.3, 5,
#'  3, 1, 6,
#'  5, -0.7, 7
#'  )
#' lin_quad_diff(tib2$a, tib2$b, -2)
#'
#' @export lin_quad_diff

lin_quad_diff <- function (x, y, x_of_int) {

  # Calculates the linear fit, extracts the fit parameters using the broom::tidy function
  # Assigns the fit parameters to the variables a1 and a2 (y = a1*x + a2)
  linear_model <- stats::lm(y ~ x)
  linear_result <- broom::tidy(linear_model)
  a1 <- dplyr::pull(dplyr::filter(linear_result, term == "x"), "estimate")
  a2 <- dplyr::pull(dplyr::filter(linear_result, term == "(Intercept)"), "estimate")

  # Calculates the quadratic fit, extracts the fit parameters using the broom::tidy function
  # Assigns the fit parameters to the variables b1, b2, and b3 (y = b1*x^2 + b2*x + b3)
  x2 <- x^2
  quadratic_model <- stats::lm(y ~ x + x2)
  quadratic_result <- broom::tidy(quadratic_model)
  b1 <- dplyr::pull(dplyr::filter(quadratic_result, term == "x2"), "estimate")
  b2 <- dplyr::pull(dplyr::filter(quadratic_result, term == "x"), "estimate")
  b3 <- dplyr::pull(dplyr::filter(quadratic_result, term == "(Intercept)"), "estimate")

  # Calculates the difference between the quadratic fit and the linear fit (quadratic -
  # linear) and returns that difference
  difference = (b1*x_of_int^2 + b2*x_of_int + b3) - (a1*x_of_int + a2)
  return(difference)
}
