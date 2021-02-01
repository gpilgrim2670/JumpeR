test_that("math_format works", {
  unformatted_values <- c("1.65m", "DNS", "1:40.65", "5-02.34")
  formatted_values <- math_format(unformatted_values)
  expect_equivalent(formatted_values, c(1.65, NA, 100.65, 62.34))

})

# testthat::test_file("tests/testthat/test-math_format_works.R")
