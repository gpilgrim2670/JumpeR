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


test_that("tf_parse pass through", {

  skip_on_cran()

  file <- "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/033-3_compiledSeries.htm"

  df_test <- file %>%
    read_results() %>%
    tf_parse()


  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15", NA), Name = c("Karel TILGA",
                                                                                    "Max VOLLMER", "Kyle GARLAND", "Leo NEUGEBAUER", "Daniel SPEJCHER",
                                                                                    "Felix WOLTER", "Ayden OWENS", "Jacob SPOTSWOOD", "Alex SPYRIDONIDIS",
                                                                                    "Peyton HAACK", "Isaiah MARTIN", "Kristo SIMULASK", "Austin WEST",
                                                                                    "Etamar BHASTEKAR", "Denim ROGERS", "Markus BALLENGEE"), Round_1 = c("15.52",
                                                                                                                                                       "15.45", "14.95", "14.95", "14.86", "12.97", "12.26", "13.01",
                                                                                                                                                       "13.47", "X", "13.24", "13.03", "12.92", "10.73", "12.22", "DNS"
                                                                                    ), Round_2 = c("14.86", "15.70", "15.41", "13.47", "14.73", "X",
                                                                                                 "12.93", "13.58", "13.19", "12.22", "13.11", "12.54", "X", "11.22",
                                                                                                 "12.19", NA), Round_3 = c("16.04", "15.82", "13.94", "14.71", "X",
                                                                                                                         "14.13", "14.07", "12.27", "13.11", "13.44", "13.33", "X", "12.60",
                                                                                                                         "12.32", "X", NA), Event = c("Hept Shot Put", "Hept Shot Put",
                                                                                                                                                      "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                      "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                      "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                      "Hept Shot Put", "Hept Shot Put"), Gender = c("Men", "Men", "Men",
                                                                                                                                                                                                    "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
                                                                                                                                                                                                    "Men", "Men", "Men", "Men"), Finals_Result = c("16.04m", "15.82m", "15.41m",
                                                                                                                                                                                                                                          "14.95m", "14.86m", "14.13m", "14.07m", "13.58m", "13.47m", "13.44m",
                                                                                                                                                                                                                                          "13.33m", "13.03m", "12.92m", "12.32m", "12.22m", "DNS"), Event_Date = structure(c(18698,
                                                                                                                                                                                                                                                                                                                             18698, 18698, 18698, 18698, 18698, 18698, 18698, 18698, 18698,
                                                                                                                                                                                                                                                                                                                             18698, 18698, 18698, 18698, 18698, 18698), class = "Date"), Team = c("Georgia",
                                                                                                                                                                                                                                                                                                                                                                                                  "Oregon", "Georgia", "Texas", "Arkansas", "Pittsburgh", "Michigan",
                                                                                                                                                                                                                                                                                                                                                                                                  "Alabama", "Auburn", "Iowa", "Purdue", "Oklahoma", "Iowa", "Arkansas",
                                                                                                                                                                                                                                                                                                                                                                                                  "Houston Baptist", "Arkansas"), Age = c("SO", "SO", "SO", "SO",
                                                                                                                                                                                                                                                                                                                                                                                                                                          "SO", "SR", "SO", "JR", "JR", "JR", "JR", "JR", "SO", "SR", "SR",
                                                                                                                                                                                                                                                                                                                                                                                                                                          "SR")), row.names = c(NA, -16L), class = "data.frame")




  expect_equivalent(df_test, df_standard)

  #### all FALSE ####

  file <-
    "https://www.flashresults.com/2019_Meets/Outdoor/06-05_NCAAOTF-Austin/015-1_compiled.htm"

  df_test <- file %>%
    read_results() %>%
    tf_parse()

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                             "20", "21", NA, NA, NA), Name = c("JuVaughn HARRISON", "Trumaine JEFFERSON",
                                                               "Yann RANDRIANASOLO", "Justin HALL", "Jacob FINCHAM-DUKES", "Jamie BROWN",
                                                               "Kemonie BRIGGS", "Jordan LATIMER", "Odaine LEWIS", "Steffin MCCARTER",
                                                               "Chris MCBRIDE", "Grant HOLLOWAY", "Charles BROWN", "Carter SHELL",
                                                               "Samory FRAGA", "R'Lazon BRUMFIELD", "Allen GORDON", "Kenan JONES",
                                                               "Jullane WALKER", "DaJuan SEWARD", "Rayvon GREY", "Keshun MCGEE",
                                                               "Jalen SEALS", "Derrick MONROE"), Event = c("Long Jump", "Long Jump",
                                                                                                           "Long Jump", "Long Jump", "Long Jump", "Long Jump", "Long Jump",
                                                                                                           "Long Jump", "Long Jump", "Long Jump", "Long Jump", "Long Jump",
                                                                                                           "Long Jump", "Long Jump", "Long Jump", "Long Jump", "Long Jump",
                                                                                                           "Long Jump", "Long Jump", "Long Jump", "Long Jump", "Long Jump",
                                                                                                           "Long Jump", "Long Jump"), Gender = c("Men", "Men", "Men", "Men",
                                                                                                                                                 "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
                                                                                                                                                 "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
                                                                                                                                                 "Men", "Men"), Wind = c("+0.7", "+1.6", "+2.3", "+0.7", "+2.0",
                                                                                                                                                                         "+1.8", "+2.1", "+3.2", "+2.1", "+1.3", "+1.0", "+1.3", "+3.1",
                                                                                                                                                                         "+2.8", "+2.7", "+1.6", "+1.4", "+1.7", "+2.0", "+2.6", "+1.6",
                                                                                                                                                                         NA, NA, NA), Finals_Result = c("8.20m", "8.18m", "8.12m", "8.05m",
                                                                                                                                                                                                        "8.00m", "7.88m", "7.87m", "7.83m", "7.78m", "7.76m", "7.73m",
                                                                                                                                                                                                        "7.72m", "7.70m", "7.67m", "7.65m", "7.62m", "7.61m", "7.54m",
                                                                                                                                                                                                        "7.44m", "7.38m", "7.38m", "FOUL", "DNS", "DNS"), Event_Date = structure(c(18053,
                                                                                                                                                                                                                                                                                   18053, 18053, 18053, 18053, 18053, 18053, 18053, 18053, 18053,
                                                                                                                                                                                                                                                                                   18053, 18053, 18053, 18053, 18053, 18053, 18053, 18053, 18053,
                                                                                                                                                                                                                                                                                   18053, 18053, 18053, 18053, 18053), class = "Date"), Team = c(NA_character_,
                                                                                                                                                                                                                                                                                                                                                 NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                 NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                 NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                 NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                 NA_character_, NA_character_, NA_character_)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                              -24L), class = "data.frame")

  expect_equivalent(df_test, df_standard)

})



# testthat::test_file("tests/testthat/test-tf_parse_works.R")
