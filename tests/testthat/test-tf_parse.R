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

test_that("tf_parse high jump with team scores", {

  skip_on_cran()

  file <- "http://tfresultsdata.deltatiming.com/2019-horizon-outdoor-championships/190503F026.htm"

  raw_results <- read_results(file)

  df_test <-
    tf_parse(
      raw_results,
      flights = TRUE,
      flight_attempts = TRUE,
      relay_athletes = TRUE
    )

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10"), Bib_Number = c("410", "370", "413", "422", "463",
                                                        "464", "314", "290", "355", "414"), Name = c("Alex Hugh-Plott",
                                                                                                     "Brady Irwin", "Salvatore Narusch", "Chase VanSickle", "Terron Taylor",
                                                                                                     "Daiquain Watson", "Bryan Keubeng", "Joseph Lietzow", "Trevor Coenen",
                                                                                                     "Marcus Nellum"), Age = c("FR", "JR", "FR", "FR", "FR", "SO",
                                                                                                                               "FR", "SO", "JR", "FR"), Team = c("OAKLAND", "MILWAUKEE", "OAKLAND",
                                                                                                                                                                 "OAKLAND", "YOUNGSTOWN ST.", "YOUNGSTOWN ST.", "ILLINOIS-CHICAGO",
                                                                                                                                                                 "DETROIT MERCY", "MILWAUKEE", "OAKLAND"), Prelims_Result = c("2.03m",
                                                                                                                                                                                                                              "2.01m", "1.95m", "2.00m", "2.03m", "1.87m", "1.90m", "1.80m",
                                                                                                                                                                                                                              "1.96m", "1.80m"), Finals_Result = c("2.03m", "2.00m", "1.97m",
                                                                                                                                                                                                                                                                   "1.97m", "1.94m", "1.94m", "1.91m", "1.88m", "1.88m", "1.85m"
                                                                                                                                                                                                                              ), DQ = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Event = c("Men High Jump",
                                                                                                                                                                                                                                                                                 "Men High Jump", "Men High Jump", "Men High Jump", "Men High Jump",
                                                                                                                                                                                                                                                                                 "Men High Jump", "Men High Jump", "Men High Jump", "Men High Jump",
                                                                                                                                                                                                                                                                                 "Men High Jump"), Flight_1 = c("1.75", "1.75", "1.75", "1.75",
                                                                                                                                                                                                                                                                                                                "1.75", "1.75", "1.75", "1.75", "1.75", "1.75"), Flight_1_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                       "-", "-", "-", "-", "O", "-", "O", "-", "O"), Flight_10 = c("2.06",
                                                                                                                                                                                                                                                                                                                                                                                                                                                   NA, NA, NA, NA, NA, NA, NA, NA, NA), Flight_10_Attempts = c("XXX",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA), Flight_2 = c("1.80", "1.80",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "1.80", "1.80", "1.80", "1.80", "1.80", "1.80", "1.80", "1.80"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ), Flight_2_Attempts = c("O", "-", "O", "O", "-", "O", "-", "O",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "O", "XO"), Flight_3 = c("1.85", "1.85", "1.85", "1.85", "1.85",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "1.85", "1.85", "1.85", "1.85", "1.85"), Flight_3_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "-", "-", "-", "O", "O", "O", "XXO", "O", "XXO"), Flight_4 = c("1.88",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.88", "1.88", "1.88", "1.88", "1.88", "1.88", "1.88", "1.88",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.88"), Flight_4_Attempts = c("O", "-", "O", "O", "-", "XO",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "-", "O", "XO", "XXX"), Flight_5 = c("1.91", "1.91", "1.91",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "1.91", "1.91", "1.91", "1.91", "1.91", "1.91", NA), Flight_5_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "O", "XO", "XO", "O", "XO", "O", "XXX", "XXX", NA), Flight_6 = c("1.94",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.94", "1.94", "1.94", "1.94", "1.94", "1.94", NA, NA, NA),
                   Flight_6_Attempts = c("XO", "XXO", "O", "XXO", "O", "O",
                                         "XXX", NA, NA, NA), Flight_7 = c("1.97", "1.97", "1.97",
                                                                          "1.97", "1.97", "1.97", NA, NA, NA, NA), Flight_7_Attempts = c("O",
                                                                                                                                         "XXO", "XO", "XO", "XXX", "XXX", NA, NA, NA, NA), Flight_8 = c("2.00",
                                                                                                                                                                                                        "2.00", "2.00", "2.00", NA, NA, NA, NA, NA, NA), Flight_8_Attempts = c("O",
                                                                                                                                                                                                                                                                               "XXO", "XXX", "XXX", NA, NA, NA, NA, NA, NA), Flight_9 = c("2.03",
                                                                                                                                                                                                                                                                                                                                          "2.03", NA, NA, NA, NA, NA, NA, NA, NA), Flight_9_Attempts = c("XO",
                                                                                                                                                                                                                                                                                                                                                                                                         "XXX", NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                -10L), class = "data.frame")

  expect_equivalent(df_test, df_standard)
})


# testthat::test_file("tests/testthat/test-tf_parse_works.R")
