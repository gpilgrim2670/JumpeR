test_that("relay athletes two lines, one athlete missing works", {
  skip_on_cran() # due to risk of external resources failing

  file <-
    "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"

  raw_data <- try(read_results(file), silent = TRUE)

  if (any(grep("error", class(raw_data)))) {
    skip("Link to external data is broken")

  } else {
    df <- tf_parse(raw_data, relay_athletes = TRUE)

    relay_athletes_test <- df[226, ] %>%
      dplyr::select(Relay_Athlete_1,
                    Relay_Athlete_2,
                    Relay_Athlete_3,
                    Relay_Athlete_4) %>%
      as.list() %>%
      unname()

    relay_athletes_standard <-
      c("Allyson Gaedje",
        "Hailey Erkkila",
        NA,
        "Reagan Bachman")

    expect_equivalent(relay_athletes_test, relay_athletes_standard)
  }

})

# testthat::test_file("tests/testthat/test-relay_athletes_works.R")
