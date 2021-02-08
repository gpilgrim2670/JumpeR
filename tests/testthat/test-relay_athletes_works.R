test_that("relay athletes two lines, one athlete missing works", {
  file <-
    "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"

  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
    expect_equal(2, 2)
  } else {
    relay_athletes_standard <-
      c("Allyson Gaedje",
        "Hailey Erkkila",
        NA,
        "Reagan Bachman")

    df <- tf_parse(read_results(file), relay_athletes = TRUE)

    relay_athletes_test <- df[226,] %>%
      dplyr::select(Relay_Athlete_1,
                    Relay_Athlete_2,
                    Relay_Athlete_3,
                    Relay_Athlete_4) %>%
      as.list() %>%
      unname()

    expect_equivalent(relay_athletes_test, relay_athletes_standard)
  }

})

# testthat::test_file("tests/testthat/test-relay_athletes_works.R")
