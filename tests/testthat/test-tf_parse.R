test_that("tf_parse heat sheet - no results", {

  skip_on_cran()

  file <- "http://tfresultsdata.deltatiming.com/2018-hurricane-invitational/180316F028.htm"

  expect_message(out <- file %>%
    read_results() %>%
    tf_parse(), "No results found in file")

  expect_null(out)

})

test_that("triple jump with attempts_split and split_attempts", {

  skip_on_cran()

  file <- "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F031.htm"

  df_test <- file %>%
    read_results() %>%
    tf_parse(
      relay_athletes = TRUE,
      rounds = TRUE,
      round_attempts = TRUE,
      split_attempts = TRUE,
      splits = TRUE
    )

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                             "20", NA, NA), Name = c("Raqib, Nisa", "Britt, Kaylah", "Austin, Nikeela",
                                                     "Taub, Shannon", "Lewis, Aaliyah", "Phoennik, Courtney", "Seto, Maris",
                                                     "McQueeney, Hope", "Holmberg, Gabby", "DeLuca, Maria", "Horrell, Rachel",
                                                     "Billone, Caroline", "Streeter, Quintasia", "Adams, Katlyn",
                                                     "Ketterman, Stefanie", "Latoza, Kayla", "Hershberger, Faith",
                                                     "Collins, Abbey", "Lodge, Emily", "Douglas, Jill", "Wagner, Alexis",
                                                     "Swartzbaugh, Lydia"), Age = c("SO", "FR", "SR", "SR", "SO",
                                                                                    "JR", "SR", "FR", "JR", "JR", "SO", "JR", "FR", "SR", "SO", "SO",
                                                                                    "FR", "SO", "FR", "SO", "JR", "SR"), Team = c("Buffalo", "Buffalo",
                                                                                                                                  "Indiana (Pa.)", "Unattached", "California (Pa.)", "John Carroll",
                                                                                                                                  "Duquesne", "Clarion", "Duquesne", "John Carroll", "Indiana (Pa.)",
                                                                                                                                  "Duquesne", "California (Pa.)", "Grove City", "Indiana (Pa.)",
                                                                                                                                  "California (Pa.)", "Indiana (Pa.)", "Grove City", "Clarion",
                                                                                                                                  "Westminster (Pa.)", "California (Pa.)", "Geneva"), Finals_Result = c("12.77m",
                                                                                                                                                                                                        "12.02m", "11.79m", "11.39m", "11.17m", "11.15m", "11.12m", "11.09m",
                                                                                                                                                                                                        "11.06m", "11.01m", "10.93m", "10.90m", "10.82m", "10.53m", "10.17m",
                                                                                                                                                                                                        "10.13m", "10.02m", "9.75m", "9.13m", "8.97m", "FOUL", "FOUL"
                                                                                                                                  ), DQ = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                            0, 0, 0, 1, 1), Event = c("Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump", "Women Triple Jump",
                                                                                                                                                                      "Women Triple Jump", "Women Triple Jump"), Round_1 = c("12.33m",
                                                                                                                                                                                                                             "11.02m", "11.46m", "11.39m", "11.04m", "10.89m", "X", "11.09m",
                                                                                                                                                                                                                             "X", "10.75m", "X", "10.79m", "X", "10.35m", "10.17m", "X", "10.02m",
                                                                                                                                                                                                                             "9.75m", "9.13m", "8.97m", NA, NA), Round_2 = c("12.36m", "X",
                                                                                                                                                                                                                                                                             "11.44m", "-", "11.07m", "11.04m", "X", "10.66m", "11.06m", "ND",
                                                                                                                                                                                                                                                                             "10.93m", "10.90m", "10.47m", "10.53m", "ND", "10.13m", "ND",
                                                                                                                                                                                                                                                                             "ND", "ND", "ND", NA, NA), Round_3 = c("12.38m", "X", "11.26m",
                                                                                                                                                                                                                                                                                                                    "-", "X", "10.86m", "11.06m", "ND", "X", "11.01m", "10.65m",
                                                                                                                                                                                                                                                                                                                    "10.81m", "10.82m", "10.43m", "X", "X", "ND", "X", "ND", "ND",
                                                                                                                                                                                                                                                                                                                    NA, NA), Round_4 = c("12.13m", "X", "11.72m", "-", "X", "11.13m",
                                                                                                                                                                                                                                                                                                                                         "X", "10.82m", "-", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                         NA, NA), Round_5 = c("12.77m", "12.02m", "11.79m", "-", "11.13m",
                                                                                                                                                                                                                                                                                                                                                              "10.76m", "X", "10.71m", "-", NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                              NA, NA, NA, NA, NA), Round_6 = c("12.27m", "X", "11.77m", "-",
                                                                                                                                                                                                                                                                                                                                                                                               "11.17m", "11.15m", "11.12m", "10.87m", "-", NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA, -22L), class = "data.frame")

  expect_equivalent(df_test, df_standard)



})

test_that("tf_parse_round_attempts", {

  file <-
    system.file("extdata",
                "Results-IVP-Track-Field-Championship-2019-20-v2.pdf",
                package = "JumpeR")

  raw_results <- read_results(file)

  df <-
    tf_parse(
      raw_results,
      rounds = TRUE,
      round_attempts = TRUE,
      relay_athletes = TRUE
    )

  total_O_round_1 <-
    sum(df$Round_1_Attempts == "O", na.rm = TRUE) # should be 17

  expect_equal(total_O_round_1, 17)
})

test_that("tf_parse mapping hytek", {

  skip_on_cran()

  links <-
    c(
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F022.htm",
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F036.htm",
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F035.htm"
    )

  df_test <- links %>%
    map(read_results) %>%
    map(tf_parse) %>%
    map(head, 3) %>%
    bind_rows()

  df_standard <-
    structure(list(Place = c("1", "2", "3", "1", "2", "3", "1", "2",
                             "3"), Team = c("Pittsburgh", "Pittsburgh", "Kent State", "Buffalo",
                                            "Youngstown St.", "Walsh", "Buffalo", "Pittsburgh", "Pittsburgh"
                             ), Finals_Result = c("3:16.26", "3:21.54", "3:21.77", "18.91m",
                                                  "17.95m", "17.57m", "17.41m", "16.47m", "16.27m"), DQ = c(0,
                                                                                                            0, 0, 0, 0, 0, 0, 0, 0), Event = c("Men 4x400 Meter Relay", "Men 4x400 Meter Relay",
                                                                                                                                               "Men 4x400 Meter Relay", "Men Weight Throw", "Men Weight Throw",
                                                                                                                                               "Men Weight Throw", "Women Weight Throw", "Women Weight Throw",
                                                                                                                                               "Women Weight Throw"), Name = c(NA, NA, NA, "Wray, Samuel", "Gutzky, Ben",
                                                                                                                                                                               "Ott, Jacob", "Hoyte, Asia", "Mallett, Shannah", "Gyles, Antoinette"
                                                                                                                                               ), Age = c(NA, NA, NA, "SR", "SR", "SO", "JR", "FR", "JR")), row.names = c(NA,
                                                                                                                                                                                                                          -9L), class = "data.frame")


  expect_equivalent(df_test, df_standard)
})

test_that("tf_parse safely mapping hytek", {

  skip_on_cran()

  links <-
    c(
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F022.htm",
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F036.htm",
      "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F035.htm"
    )

  df_test <- links %>%
    map(read_results) %>%
    map(
      purrr::safely(tf_parse, otherwise = NA),
      relay_athletes = TRUE,
      rounds = TRUE,
      round_attempts = TRUE,
      split_attempts = TRUE,
      splits = TRUE
    ) %>%
    SwimmeR::discard_errors() %>%
    map(head, 3) %>%
    bind_rows()

  df_standard <-
    structure(list(Place = c("1", "2", "3", "1", "2", "3", "1", "2",
                             "3"), Team = c("Pittsburgh", "Pittsburgh", "Kent State", "Buffalo",
                                            "Youngstown St.", "Walsh", "Buffalo", "Pittsburgh", "Pittsburgh"
                             ), Finals_Result = c("3:16.26", "3:21.54", "3:21.77", "18.91m",
                                                  "17.95m", "17.57m", "17.41m", "16.47m", "16.27m"), DQ = c(0,
                                                                                                            0, 0, 0, 0, 0, 0, 0, 0), Event = c("Men 4x400 Meter Relay", "Men 4x400 Meter Relay",
                                                                                                                                               "Men 4x400 Meter Relay", "Men Weight Throw", "Men Weight Throw",
                                                                                                                                               "Men Weight Throw", "Women Weight Throw", "Women Weight Throw",
                                                                                                                                               "Women Weight Throw"), Name = c(NA, NA, NA, "Wray, Samuel", "Gutzky, Ben",
                                                                                                                                                                               "Ott, Jacob", "Hoyte, Asia", "Mallett, Shannah", "Gyles, Antoinette"
                                                                                                                                               ), Age = c(NA, NA, NA, "SR", "SR", "SO", "JR", "FR", "JR"), Round_1 = c(NA,
                                                                                                                                                                                                                       NA, NA, "17.01m", "ND", "17.57m", "ND", "16.47m", "15.36m"),
                   Round_2 = c(NA, NA, NA, "18.06m", "17.43m", "ND", "ND", "16.12m",
                               "16.27m"), Round_3 = c(NA, NA, NA, "ND", "17.59m", "15.98m",
                                                      "14.71m", "16.42m", "ND"), Round_4 = c(NA, NA, NA, "18.91m",
                                                                                             "17.39m", "ND", "17.41m", "16.31m", "ND"), Round_5 = c(NA,
                                                                                                                                                    NA, NA, "18.33m", "17.68m", "17.29m", "15.01m", "15.17m",
                                                                                                                                                    "ND"), Round_6 = c(NA, NA, NA, "18.86m", "17.95m", "ND",
                                                                                                                                                                       "ND", "15.37m", "15.68m")), row.names = c(NA, -9L), class = "data.frame")

  expect_equivalent(df_test, df_standard)
})

test_that("tf_parse_rounds", {
  file <-
    system.file("extdata", "underdistance-2020-result.pdf", package = "JumpeR")

  raw_results <- read_results(file)

  df <-
    tf_parse(
      raw_results,
      rounds = TRUE,
      round_attempts = TRUE,
      relay_athletes = TRUE
    )

  total_FOUL_round_1 <-
    sum(df$Round_1 == "FOUL", na.rm = TRUE) # should be 8
  total_FOUL_round_2 <-
    sum(df$Round_2 == "FOUL", na.rm = TRUE) # should be 9
  total_FOUL_round_3 <-
    sum(df$Round_3 == "FOUL", na.rm = TRUE) # should be 6
  total_FOUL_round_4 <-
    sum(df$Round_4 == "FOUL", na.rm = TRUE) # should be 10
  total_FOUL_round_5 <-
    sum(df$Round_5 == "FOUL", na.rm = TRUE) # should be 11
  total_FOUL_round_6 <-
    sum(df$Round_6 == "FOUL", na.rm = TRUE) # should be 9

  FOUL_list <-
    c(
      total_FOUL_round_1,
      total_FOUL_round_2,
      total_FOUL_round_3,
      total_FOUL_round_4,
      total_FOUL_round_5,
      total_FOUL_round_6
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
        rounds = TRUE,
        round_attempts = TRUE,
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
      rounds = TRUE,
      round_attempts = TRUE,
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
                                                                                                                                                                                                                                                                                 "Men High Jump"), Round_1 = c("1.75", "1.75", "1.75", "1.75",
                                                                                                                                                                                                                                                                                                                "1.75", "1.75", "1.75", "1.75", "1.75", "1.75"), Round_1_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                       "-", "-", "-", "-", "O", "-", "O", "-", "O"), Round_10 = c("2.06",
                                                                                                                                                                                                                                                                                                                                                                                                                                                   NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_10_Attempts = c("XXX",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_2 = c("1.80", "1.80",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "1.80", "1.80", "1.80", "1.80", "1.80", "1.80", "1.80", "1.80"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ), Round_2_Attempts = c("O", "-", "O", "O", "-", "O", "-", "O",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "O", "XO"), Round_3 = c("1.85", "1.85", "1.85", "1.85", "1.85",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "1.85", "1.85", "1.85", "1.85", "1.85"), Round_3_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "-", "-", "-", "O", "O", "O", "XXO", "O", "XXO"), Round_4 = c("1.88",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.88", "1.88", "1.88", "1.88", "1.88", "1.88", "1.88", "1.88",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.88"), Round_4_Attempts = c("O", "-", "O", "O", "-", "XO",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "-", "O", "XO", "XXX"), Round_5 = c("1.91", "1.91", "1.91",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "1.91", "1.91", "1.91", "1.91", "1.91", "1.91", NA), Round_5_Attempts = c("-",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "O", "XO", "XO", "O", "XO", "O", "XXX", "XXX", NA), Round_6 = c("1.94",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1.94", "1.94", "1.94", "1.94", "1.94", "1.94", NA, NA, NA),
                   Round_6_Attempts = c("XO", "XXO", "O", "XXO", "O", "O",
                                         "XXX", NA, NA, NA), Round_7 = c("1.97", "1.97", "1.97",
                                                                          "1.97", "1.97", "1.97", NA, NA, NA, NA), Round_7_Attempts = c("O",
                                                                                                                                         "XXO", "XO", "XO", "XXX", "XXX", NA, NA, NA, NA), Round_8 = c("2.00",
                                                                                                                                                                                                        "2.00", "2.00", "2.00", NA, NA, NA, NA, NA, NA), Round_8_Attempts = c("O",
                                                                                                                                                                                                                                                                               "XXO", "XXX", "XXX", NA, NA, NA, NA, NA, NA), Round_9 = c("2.03",
                                                                                                                                                                                                                                                                                                                                          "2.03", NA, NA, NA, NA, NA, NA, NA, NA), Round_9_Attempts = c("XO",
                                                                                                                                                                                                                                                                                                                                                                                                         "XXX", NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                -10L), class = "data.frame")

  expect_equivalent(df_test, df_standard)
})


test_that("tf_parse pass through", {

  skip_on_cran()


  #### Hept Shot Put ####

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
                                                                                    "Etamar BHASTEKAR", "Denim ROGERS", "Markus BALLENGEE"), Event = c("Hept Shot Put",
                                                                                                                                                       "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                       "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                       "Hept Shot Put", "Hept Shot Put", "Hept Shot Put", "Hept Shot Put",
                                                                                                                                                       "Hept Shot Put", "Hept Shot Put", "Hept Shot Put"), Gender = c("Men",
                                                                                                                                                                                                                      "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
                                                                                                                                                                                                                      "Men", "Men", "Men", "Men", "Men", "Men"), Finals_Result = c("16.04m",
                                                                                                                                                                                                                                                                                   "15.82m", "15.41m", "14.95m", "14.86m", "14.13m", "14.07m", "13.58m",
                                                                                                                                                                                                                                                                                   "13.47m", "13.44m", "13.33m", "13.03m", "12.92m", "12.32m", "12.22m",
                                                                                                                                                                                                                                                                                   "DNS"), Event_Date = structure(c(18698, 18698, 18698, 18698,
                                                                                                                                                                                                                                                                                                                    18698, 18698, 18698, 18698, 18698, 18698, 18698, 18698, 18698,
                                                                                                                                                                                                                                                                                                                    18698, 18698, 18698), class = "Date"), Team = c("Georgia", "Oregon",
                                                                                                                                                                                                                                                                                                                                                                    "Georgia", "Texas", "Arkansas", "Pittsburgh", "Michigan", "Alabama",
                                                                                                                                                                                                                                                                                                                                                                    "Auburn", "Iowa", "Purdue", "Oklahoma", "Iowa", "Arkansas", "Houston Baptist",
                                                                                                                                                                                                                                                                                                                                                                    "Arkansas"), Round_1 = c("15.52", "15.45", "14.95", "14.95",
                                                                                                                                                                                                                                                                                                                                                                                             "14.86", "12.97", "12.26", "13.01", "13.47", "X", "13.24", "13.03",
                                                                                                                                                                                                                                                                                                                                                                                             "12.92", "10.73", "12.22", "DNS"), Round_2 = c("14.86", "15.70",
                                                                                                                                                                                                                                                                                                                                                                                                                                            "15.41", "13.47", "14.73", "X", "12.93", "13.58", "13.19", "12.22",
                                                                                                                                                                                                                                                                                                                                                                                                                                            "13.11", "12.54", "X", "11.22", "12.19", NA), Round_3 = c("16.04",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "15.82", "13.94", "14.71", "X", "14.13", "14.07", "12.27", "13.11",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "13.44", "13.33", "X", "12.60", "12.32", "X", NA), Age = c("SO",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "SO", "SO", "SO", "SO", "SR", "SO", "JR", "JR", "JR", "JR", "JR",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "SO", "SR", "SR", "SR")), row.names = c(NA, -16L), class = "data.frame")
  expect_equivalent(df_test, df_standard)

  #### Long Jump ####

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

test_that("tf_parse 1600m with team scores at bottom", {

  skip_on_cran()

  file <- "http://results.deltatiming.com/tf/2019-hurricane-invitational/190315F076"

  df_test <- file %>%
    read_results() %>%
    tf_parse()

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13"), Name = c("Robert Pedroza", "Sukeil Foucha",
                                                                    "Will Noonan", "Alec Collado", "Humberto Ramirez", "Zachary Vincennie",
                                                                    "Kevin Kergean", "James Cusack", "George Stark", "Pedro Garcia",
                                                                    "Tomas Esber", "Lucas Nolasco", "Lucas Batista"), Age = c("JR",
                                                                                                                              "SR", "SR", "SR", "SO", "JR", "JR", "SR", "FR", "JR", "JR", "JR",
                                                                                                                              "SR"), Team = c("Key West HS", "Piper", "Spanish River", "Miami Coral Park",
                                                                                                                                              "Piper", "Spanish River", "Miami Columbus", "Ransom Everglades",
                                                                                                                                              "Palmer Trinity", "Braddock", "Ransom Everglades", "Cypress Bay",
                                                                                                                                              "Miami Coral Park"), Finals_Result = c("4:19.69", "4:25.06",
                                                                                                                                                                                     "4:25.56", "4:27.56", "4:28.72", "4:29.69", "4:32.37", "4:33.01",
                                                                                                                                                                                     "4:34.06", "4:38.01", "4:42.45", "4:42.76", "5:23.53"), DQ = c(0,
                                                                                                                                                                                                                                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Event = c("Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School",
                                                                                                                                                                                                                                                                                                   "Boys 1600m Run Invitational High School", "Boys 1600m Run Invitational High School"
                                                                                                                                                                                                                                                    )), row.names = c(NA, -13L), class = "data.frame")

    expect_equivalent(df_test, df_standard)
})


test_that("hytek_parse long jump with wind speeds by round/attempt", {

  skip_on_cran()

  file <- "http://results.deltatiming.com/ncaa/tf/2019-florida-relays/print/190328F005"

  df_test <- file %>%
    read_results() %>%
    tf_parse(rounds = TRUE)

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15", "16", "17"), Name = c("Alyssa Jones",
                                                                                            "Caroline Johnston", "Eddiyah Frye", "Destiny Castillo", "Morgan Bentley",
                                                                                            "Jordyn Showers", "Jasmyn Dorsey", "Christon Kingcade", "Samarah Hill",
                                                                                            "Ashantae Harvey", "Jaela Hollie", "Natiya James", "Saniah Fuller",
                                                                                            "Mackenzie Forrest", "Ameion Hamlet", "Gabriele Matthews", "Jiya Hastings"
                             ), Age = c("FR", "SR", "SO", "SR", "11", "JR", "JR", "SR", "SR",
                                        "JR", "SR", "JR", "SR", "JR", "JR", "JR", "SO"), Team = c("Miami Southridge",
                                                                                                  "Episcopal", "St. Thomas Aquinas", "Hallandale", "Brookwood",
                                                                                                  "Calvary Chri", "Mainland", "Miami Northwestern", "Trinity Prep",
                                                                                                  "Hallandale", "Jones", "Eastside", "Windermere", "Forest", "Godby",
                                                                                                  "Dr. Phillips", "DeLand"), Finals_Result = c("6.05m", "5.92m",
                                                                                                                                               "5.83m", "5.80m", "5.66m", "5.65m", "5.41m", "5.38m", "5.36m",
                                                                                                                                               "5.30m", "5.28m", "5.14m", "5.10m", "5.01m", "4.94m", "4.52m",
                                                                                                                                               "4.50m"), DQ = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                                0, 0), Event = c("Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School", "Girls Long Jump High School",
                                                                                                                                                                                 "Girls Long Jump High School"), Round_1 = c("5.90m", "FOUL",
                                                                                                                                                                                                                              "FOUL", "FOUL", "5.53m", "5.64m", "FOUL", "5.38m", "FOUL", "5.12m",
                                                                                                                                                                                                                              "5.28m", "4.98m", "FOUL", "4.61m", "4.94m", "4.52m", "4.50m"),
                   Round_1_Wind = c("0.2", "1.9", "1.7", "2.7", "0.5", "1.4",
                                     "+0.0", "0.6", "0.5", "0.7", "0.3", "0.6", "2.3", "+0.0",
                                     "3.4", "1.9", "0.5"), Round_2 = c("6.05m", "5.65m", "FOUL",
                                                                        "5.80m", "FOUL", "5.65m", "5.41m", "ND", "5.24m", "5.30m",
                                                                        "5.09m", "5.14m", "4.85m", "ND", "FOUL", "ND", "ND"), Round_2_Wind = c("2.2",
                                                                                                                                                "+0.0", "0.2", "2.3", "1.7", "1.6", "+0.0", "2.1", "0.8",
                                                                                                                                                "1.0", "1.2", "0.7", "1.5", "1.7", "+0.0", "1.0", "0.2"),
                   Round_3 = c("5.93m", "FOUL", "5.83m", "ND", "5.49m", "5.26m",
                                "FOUL", "ND", "5.36m", "5.23m", "ND", "ND", "5.10m", "5.01m",
                                "FOUL", "ND", "FOUL"), Round_3_Wind = c("0.2", "0.2", "2.1",
                                                                         "+0.0", "1.1", "0.2", "1.1", "0.2", "1.9", "1.2", "1.7",
                                                                         "2.2", "0.5", "+0.0", "2.2", "0.3", "1.0"), Round_4 = c("5.93m",
                                                                                                                                  "5.72m", "5.29m", "FOUL", "FOUL", "5.43m", "5.23m", "5.18m",
                                                                                                                                  "5.29m", NA, NA, NA, NA, NA, NA, NA, NA), Round_4_Wind = c("1.7",
                                                                                                                                                                                              "0.8", "1.2", "0.2", "0.9", "0.6", "2.4", "0.9", "2.2", NA,
                                                                                                                                                                                              NA, NA, NA, NA, NA, NA, NA), Round_5 = c("5.74m", "5.72m",
                                                                                                                                                                                                                                        "5.81m", "FOUL", "FOUL", "5.46m", "5.14m", "FOUL", "5.16m",
                                                                                                                                                                                                                                        NA, NA, NA, NA, NA, NA, NA, NA), Round_5_Wind = c("1.5",
                                                                                                                                                                                                                                                                                           "0.9", "+0.0", "+0.0", "0.9", "1.2", "+0.0", "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                           NA, NA, NA, NA, NA, NA, NA, NA), Round_6 = c("5.82m", "5.92m",
                                                                                                                                                                                                                                                                                                                                         "FOUL", "FOUL", "5.66m", "5.24m", "5.13m", "5.06m", "5.14m",
                                                                                                                                                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA), Round_6_Wind = c("2.1",
                                                                                                                                                                                                                                                                                                                                                                                            "3.0", "1.4", "1.5", "3.4", "1.6", "+0.0", "1.9", "1.4",
                                                                                                                                                                                                                                                                                                                                                                                            NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA, -17L), class = "data.frame")


  expect_equivalent(df_test, df_standard)
})


test_that("hytek_parse long jump with wind speeds by round/attempt, missing speeds", {

  skip_on_cran()

  file <- "http://results.deltatiming.com/tf/2019-tiger-track-classic/190405F032"

  df_test <- file %>%
    read_results() %>%
    tf_parse(rounds = TRUE)

  df_standard <-
    structure(list(Place = c("1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", NA, NA, NA, NA), Name = c("Brandon Roulhac",
                                                                                          "Jamie Brown", "Michael Pichay", "Elijah White", "Willington Wright",
                                                                                          "Jacob Patten", "Benjamin Okafor", "Tyler Hanks", "Jordan Wallace",
                                                                                          "Marcus Cade", "DeAndre Baker", "Jordan Simons", "Nicholas Lyons",
                                                                                          "Eric Durham", "Philippe St-Hilaire", "John Warren", "Fabian Edoki",
                                                                                          "Devonta Brassell"), Age = c(NA, "SR", "SO", "SO", "FR", "SR",
                                                                                                                       "FR", "SO", "FR", "FR", "SR", "SO", "JR", "SR", NA, "SR", "JR",
                                                                                                                       "SR"), Team = c("Unattached", "Alabama State", "Holy Cross",
                                                                                                                                       "Maine", "Mid. Tenn. State", "Mid. Tenn. State", "Western Carolina",
                                                                                                                                       "Western Carolina", "Troy", "Alabama State", "Alabama A&M", "Alabama A&M",
                                                                                                                                       "Western Carolina", "Troy", "Unattached", "Southern Miss.", "Mid. Tenn. State",
                                                                                                                                       "Southern Miss."), Finals_Result = c("16.01m", "14.93m", "14.73m",
                                                                                                                                                                            "14.69m", "14.64m", "14.60m", "14.45m", "13.84m", "13.79m", "13.77m",
                                                                                                                                                                            "13.74m", "13.64m", "13.44m", "13.30m", "DNS", "DNS", "DNS",
                                                                                                                                                                            "DNS"), Wind_Speed = c("+0.0", NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                   NA, NA, NA, NA, NA, "NWI", "NWI", "NWI", "NWI"), DQ = c(0, 0,
                                                                                                                                                                                                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Event = c("Men Triple Jump",
                                                                                                                                                                                                                                                                                                                      "Men Triple Jump", "Men Triple Jump", "Men Triple Jump", "Men Triple Jump",
                                                                                                                                                                                                                                                                                                                      "Men Triple Jump", "Men Triple Jump", "Men Triple Jump", "Men Triple Jump",
                                                                                                                                                                                                                                                                                                                      "Men Triple Jump", "Men Triple Jump", "Men Triple Jump", "Men Triple Jump",
                                                                                                                                                                                                                                                                                                                      "Men Triple Jump", "Men Triple Jump", "Men Triple Jump", "Men Triple Jump",
                                                                                                                                                                                                                                                                                                                      "Men Triple Jump"), Round_1 = c("15.75m", "X", "X", "14.64m",
                                                                                                                                                                                                                                                                                                                                                       "14.33m", "14.40m", "14.09m", "13.50m", "X", "13.31m", "13.74m",
                                                                                                                                                                                                                                                                                                                                                       "13.62m", "13.44m", "X", NA, NA, NA, NA), Round_1_Wind = c("+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                   "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "-0.3", "-0.1",
                                                                                                                                                                                                                                                                                                                                                                                                                   "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", NA, NA, NA, NA), Round_2 = c("16.01m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "X", "14.73m", "14.66m", "14.31m", "14.60m", "14.45m", "13.84m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "13.39m", "X", "X", "13.64m", "13.15m", "13.30m", NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         NA), Round_2_Wind = c("+0.0", "+0.0", "+0.0", "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "+0.0", "+0.0", "+0.0", "-0.1", "-0.6", "+0.0", "-1.4", "-0.8",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "-1.1", NA, NA, NA, NA), Round_3 = c("X", "14.91m", "X", "14.69m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "14.28m", "X", "14.25m", "X", "13.79m", "13.77m", "X", "X", "X",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "X", NA, NA, NA, NA), Round_3_Wind = c("+0.0", "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "+0.0", "+0.0", "+0.0", "-0.9", "+0.0", "+0.0", "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "+0.0", "+0.0", "+0.0", NA, NA, NA, NA), Round_4 = c("15.91m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "14.34m", "14.34m", "13.43m", "14.64m", "14.18m", "13.52m", "13.74m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "13.21m", NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_4_Wind = c("+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "0.3",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_5 = c("15.85m", "X",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "14.14m", "14.56m", "14.60m", "13.94m", "X", "13.68m", "13.76m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_5_Wind = c("+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "0.2", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              NA, NA, NA, NA, NA, NA, NA, NA, NA), Round_6 = c("14.74m", "14.93m",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "14.31m", "X", "14.62m", "14.48m", "13.53m", "X", "X", NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NA, NA, NA, NA, NA, NA, NA), Round_6_Wind = c(NA, "+0.0", "+0.0",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", "+0.0", NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               NA, NA, NA, NA, NA)), row.names = c(NA, -18L), class = "data.frame")


  expect_equivalent(df_test, df_standard)
})


# testthat::test_file("tests/testthat/test-tf_parse_works.R")
