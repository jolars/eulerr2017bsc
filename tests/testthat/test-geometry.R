context("test-geometry.R")

test_that("multiplication works", {
  expect_type(ellipse_arc(c(1, 1), c(0.5, 0.3), pi/3, 300, c(pi/8, pi)),
              "double")
})
