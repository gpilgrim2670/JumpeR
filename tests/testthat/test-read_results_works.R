test_that("multiplication works", {
  file <-
    system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")

  x <- read_results(file)
})
