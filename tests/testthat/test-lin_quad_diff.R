# tests that the function returns the correct value with a valid input

d2 = 0.165178571428570

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

xoi2 = -2

testthat::test_that("Correct value", {
  expect_equal(d2, lin_quad_diff(tib2$a, tib2$b, xoi2))
})

# tests that the function returns an error when the input vectors differ in length

v1 = c(1, 2, 3, 4, 5)
v2 = c(2, 3, 4, 5, 6, 7)

testthat::test_that("Mismatch vector lengths", {
  expect_error(lin_quad_diff(v1, v2, xoi2))
})

# tests that the function returns an error when the x of interest value is non-numeric

xoi3 = "blue"

testthat::test_that("x_of_int is not a number", {
  expect_error(lin_quad_diff(tib2$a, tib2$b, xoi3))
})

rm("d2")
rm("tib2")
rm("xoi2")
rm("v1")
rm("v2")
rm("xoi3")
