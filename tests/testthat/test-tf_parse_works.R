test_that("tf_parse_flight_attempts", {

  skip_on_cran()

  file <-
    system.file("extdata",
                "Results-IVP-Track-Field-Championship-2019-20-v2.pdf",
                package = "JumpeR")

  raw_results <- read_results(file)

  df <-
    tf_parse(
      raw_results,
      flights = TRUE,
      flight_attempts = TRUE,
      relay_athletes = TRUE
    )

  total_O_flight_1 <-
    sum(df$Flight_1_Attempts == "O", na.rm = TRUE) # should be 17

  expect_equal(total_O_flight_1, 17)
})

test_that("tf_parse_flights", {
  file <-
    system.file("extdata", "underdistance-2020-result.pdf", package = "JumpeR")

  raw_results <- read_results(file)

  df <-
    tf_parse(
      raw_results,
      flights = TRUE,
      flight_attempts = TRUE,
      relay_athletes = TRUE
    )

  total_FOUL_flight_1 <-
    sum(df$Flight_1 == "FOUL", na.rm = TRUE) # should be 8
  total_FOUL_flight_2 <-
    sum(df$Flight_2 == "FOUL", na.rm = TRUE) # should be 9
  total_FOUL_flight_3 <-
    sum(df$Flight_3 == "FOUL", na.rm = TRUE) # should be 6
  total_FOUL_flight_4 <-
    sum(df$Flight_4 == "FOUL", na.rm = TRUE) # should be 10
  total_FOUL_flight_5 <-
    sum(df$Flight_5 == "FOUL", na.rm = TRUE) # should be 11
  total_FOUL_flight_6 <-
    sum(df$Flight_6 == "FOUL", na.rm = TRUE) # should be 9

  FOUL_list <-
    c(
      total_FOUL_flight_1,
      total_FOUL_flight_2,
      total_FOUL_flight_3,
      total_FOUL_flight_4,
      total_FOUL_flight_5,
      total_FOUL_flight_6
    )

  expect_equal(FOUL_list, c(8, 9, 6, 10, 11, 9))
})


test_that("tf_parse_standard", {

  skip_on_cran() # due to time, risk of external resources failing

  # import standard
  df_standard <- readRDS(system.file("extdata", "df_standard.rds", package = "JumpeR"))

  # import test files
  file_1 <-
     system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR") # good
  file_2 <- system.file("extdata", "underdistance-2020-result.pdf", package = "JumpeR") # good
  file_3 <-
    system.file("extdata", "SMTFA-2019-Full-Results.pdf", package = "JumpeR") # good
  file_4 <-
    system.file("extdata", "sa-performance-trial-1-day-1-results.pdf", package = "JumpeR") # good
  url_1 <- "http://results.yentiming.com/2019/Indoor/12-21-18-west.htm" # good
  # url_2 <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"

  # test external links
  # if(sum(sapply(c(url_1, url_2), is_link_broken)) > 0.9){
  raw_data <- try(read_results(url_1), silent = TRUE)

  if(any(grep("error", class(raw_data)))){
    skip("Link to external data is broken")
    suppressWarnings(closeAllConnections())
  } else {

  # list of sources
  sources <- c(file_1,
               file_2,
               file_3,
               file_4)
               # url_2)

  # helper function to apply read_results across list of links
  Read_Map <- function(links) {

    scrape_test_all <-
      purrr::map(links, read_results, node = "pre")

    names(scrape_test_all) <- links
    return(scrape_test_all)

  }

  # helper function to apply swim_parse across all files
  Parse_Map <- function(links) {

    all_results <-
      purrr::map(links, tf_parse)

    return(all_results)

  }

  # get test data to compare with standard
  suppressWarnings(df_test <- Read_Map(sources))
  suppressWarnings(df_test$url_1 <- raw_data)
  df_test <- Parse_Map(df_test)
  df_test <- dplyr::bind_rows(df_test, .id = "source") %>%
    dplyr::select(-source)

  # to regenerate df_standard if df_test is more correct
  # windows
  # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard.rds")
  # mac
  # readr::write_rds(df_test, "inst/extdata/df_standard.rds")
  # to compare results
  # df <- dplyr::anti_join(df_standard, df_test)

  # compare standard to test
  expect_equivalent(df_standard,
                    df_test)
  }
})

test_that("tf_parse_attempts_splits_works", {

  skip_on_cran() # due to risk of external resources failing

  url_2 <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"

  raw_data <- try(read_results(url_2), silent = TRUE)

  if(any(grep("error", class(raw_data)))){
    skip("Link to external data is broken")
    suppressWarnings(closeAllConnections())
  } else {


    df_standard_polevault_hytek <- readRDS(system.file("extdata", "df_standard_polevault_hytek.rds", package = "JumpeR"))

    df <-
      tf_parse(
        raw_data,
        flights = TRUE,
        flight_attempts = TRUE,
        split_attempts = TRUE
      )

  df_test <- df %>%
    dplyr::filter(Event == "Men Pole Vault")

  expect_equivalent(df_test, df_standard_polevault_hytek)

  # to regenerate df_standard if df_test is more correct
  # windows
  # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard_polevault_hytek.rds")
  # mac
  # readr::write_rds(df_test, "inst/extdata/df_standard_polevault_hytek.rds")
  # to compare results
  # df <- dplyr::anti_join(df_standard_polevault, df_test)

  }
})

# testthat::test_file("tests/testthat/test-tf_parse_works.R")
