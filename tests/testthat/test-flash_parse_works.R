test_that("flash parse works sprint", {

  file <-
    system.file("extdata", "flash_sprint.pdf", package = "JumpeR")

  raw_data <- read_results(file)

    # generate standard
    df_standard <- data.frame(
      Place = as.character(seq(1, 15, 1)),
      Name = c(
        "Grant HOLLOWAY",
        "Daniel ROBERTS",
        "Isaiah MOORE",
        "Damion THOMAS",
        "Tai BROWN",
        "Michael NICHOLLS",
        "Shakiel CHATTOO",
        "Carl ELLIOTT III",
        "Robert DUNNING",
        "Caleb WILT",
        "Arthur PRICE",
        "Cory POOLE",
        "Kenney BROADNAX",
        "Tre'Bien GILBERT",
        "Craig CLARK"
      ),
      Age = c(
        "JR",
        "JR",
        "SR",
        "SO",
        "FR",
        "SR",
        "SR",
        NA,
        "JR",
        "SR",
        "SR",
        "SO",
        "FR",
        "FR",
        "SO"
      ),
      Team = c(
        "FLORIDA",
        "KENTUCKY",
        "SOUTH CAROLINA",
        "LSU",
        "KENTUCKY",
        "GEORGIA",
        "ARKANSAS",
        "ARKANSAS",
        "ALABAMA",
        "KENTUCKY",
        "LSU",
        "FLORIDA",
        "OLE MISS",
        "ARKANSAS",
        "AUBURN"
      ),
      Finals_Result = c(
        "13.07",
        "13.31",
        "13.49",
        "13.57",
        "13.62",
        "13.64",
        "13.79",
        "13.79",
        "13.83",
        "13.85",
        "13.89",
        "13.91",
        "13.99",
        "14.30",
        "14.36"
      ),
      Wind_Speed = c(
        "+0.8",
        "+0.8",
        "+0.9",
        "+0.9",
        "+0.8",
        "+0.8",
        "+0.9",
        "+0.9",
        "+0.8",
        "+0.9",
        "+0.9",
        "+0.8",
        "+0.9",
        "+0.8",
        "+0.8"
      ),
      Tiebreaker = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        "13.784",
        "13.786",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA ,
        NA
      ),
      DQ = rep(0, 15),
      Event = rep("Men 110 M Hurdles Prelims 6:00 PM 10 May 2019", 15),
      row.names = NULL,
      stringsAsFactors = FALSE
    )

    # generate test df
    suppressWarnings(
      df_test <-
        tf_parse(
          raw_data,
          flights = TRUE,
          flight_attempts = TRUE,
          relay_athletes = TRUE
        )
    )

    # test
    expect_equivalent(df_standard,
                      df_test)

})

test_that("flash parse works long jump, team scores in results", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf"

  raw_data <- try(read_results(file), silent = TRUE)

  if (any(grep("error", class(raw_data)))) {
    skip("Link to external data is broken")
  } else {
    # import standard
    df_standard_longjump <-
      readRDS(system.file("extdata", "df_standard_longjump.rds", package = "JumpeR"))

    # results realy do contain "ST. JOSEPH'S (PA" with a missing ")"

    # generate test df
    suppressWarnings(
      df_test <-
        tf_parse(
          raw_data,
          flights = TRUE,
          flight_attempts = TRUE,
          relay_athletes = TRUE
        )
    )

    # test
    expect_equivalent(df_standard_longjump,
                      df_test)

    # to regenerate df_standard if df_test is more correct
    # windows
    # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard_longjump.rds")
    # mac
    # readr::write_rds(df_test, "inst/extdata/df_standard_longjump.rds")
    # to compare results
    # df <- dplyr::anti_join(df_standard_longjump, df_test)
  }
})

test_that("flash parse works pole vault", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2018_Meets/Outdoor/04-20_VirginiaChallenge/034-1.pdf"

  raw_data <- try(read_results(file), silent = TRUE)

  if (any(grep("error", class(raw_data)))) {
    skip("Link to external data is broken")
  } else {
    # import standard
    df_standard_polevault <-
      readRDS(system.file("extdata", "df_standard_polevault.rds", package = "JumpeR"))

    # results really do contain "ST. JOSEPH'S (PA" with a missing ")"

    # generate test df
    suppressWarnings(
      df_test <-
        tf_parse(
          raw_data,
          flights = TRUE,
          flight_attempts = TRUE,
          relay_athletes = TRUE
        )
    )

    # test
    expect_equivalent(df_standard_polevault,
                      df_test)

    # to regenerate df_standard if df_test is more correct
    # windows
    # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard_polevault.rds")
    # mac
    # readr::write_rds(df_test, "inst/extdata/df_standard_polevault.rds")
    # to compare results
    # df <- dplyr::anti_join(df_standard_polevault, df_test)

  }
})

test_that("flash parse works relay, team scores in results", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/032-1.pdf"

  raw_data <- try(read_results(file), silent = TRUE)

  if (any(grep("error", class(raw_data)))) {
    skip("Link to external data is broken")
  } else {
    # generate standard
    df_standard <-
      data.frame(
        Place = c(as.character(seq(1, 9, 1)), "DNF"),
        Team = c(
          "GEORGE MASON",
          "VCU",
          "RHODE ISLAND",
          "DUQUESNE",
          "ST. JOSEPH'S (PA.)",
          "SAINT LOUIS",
          "DAVIDSON",
          "UMASS AMHERST",
          "FORDHAM",
          "LA SALLE"
        ),
        Finals_Result = c(
          "3:09.10",
          "3:09.15",
          "3:09.95",
          "3:13.42",
          "3:14.87",
          "3:16.06",
          "3:18.64",
          "3:20.43",
          "3:21.07",
          "DNF"
        ),
        DQ = c(rep(0, 9), 1),
        Event = rep("Men 4x400 M Relay 2:55 PM 6 May 2018", 10)
      )

    # generate test df
    suppressMessages(
      df_test <-
        tf_parse(
          raw_data,
          flights = TRUE,
          flight_attempts = TRUE,
          relay_athletes = TRUE
        )
    )

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

#### test causes problems on debian builds, maybe due to pdftools issues - not sure ####
# test_that("flash parse works sprint, diamond league with some birthdates", {
#
#   skip_on_cran() # due to risk of external resources failing
#
#   file <-
#     "https://www.flashresults.com/2019_Meets/Outdoor/06-30_PreClassic/002-1.pdf"
#
# raw_data <- try(read_results(file), silent = TRUE)
#
# if(any(grep("error", class(raw_data)))){
#   skip("Link to external data is broken")
# } else {
#     # generate standard
#     df_standard <- data.frame(Place = as.character(seq(1, 6, 1)),
#                               Name = c("Arian SMITH", "Marcellus MOORE", "Ryan MARTIN", "Ryan MULHOLLAND", "Lance BROOME", "Tyrese COOPER"),
#                               Team = rep("USA", 6),
#                               Finals_Result = c("10.41", "10.46", "10.56", "10.60", "10.63", "10.64"),
#                               DQ = rep(0, 6),
#                               Birthdate = c(NA, NA, "21-Sep-2001", NA, NA, "21-Mar-2000"),
#                               Event = rep("Boys 100 M High School 12:46 30 Jun 2019", 6))
#
#     # generate test df
#     df_test <- tf_parse(
#         raw_data,
#         flights = TRUE,
#         flight_attempts = TRUE,
#         relay_athletes = TRUE
#       )
#
#     # test
#     expect_equivalent(df_standard,
#                       df_test)
#   }
# })

test_that("flash parse works split_attempts single line", {

  skip_on_cran()

  file <-
    system.file("extdata", "flash_polevault_splits.pdf", package = "JumpeR")

  raw_data <- read_results(file)

    # generate standard
    df_standard_polevault_splits <-
      readRDS(system.file("extdata", "df_standard_polevault_splits.rds", package = "JumpeR"))

    # generate test df
    suppressWarnings(
      df_test <- tf_parse(
        raw_data,
        flights = TRUE,
        flight_attempts = TRUE,
        relay_athletes = TRUE,
        split_attempts = TRUE
      )
    )

    # test
    expect_equivalent(df_standard_polevault_splits,
                      df_test)

    # to regenerate df_standard if df_test is more correct
    # windows
    # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard_polevault_splits.rds")
    # mac
    # readr::write_rds(df_test, "inst/extdata/df_standard_polevault_splits.rds")
    # to compare results
    # df <- dplyr::anti_join(df_standard_polevault_splits, df_test)

})

#### this test works when called but not not work within devtools::check and I don't know why ####

# test_that("flash parse works split_attempts double line", {
#
# skip_on_cran() # due to risk of external resources failing
#
#   file <-
#     "https://www.flashresults.com/2020_Meets/Indoor/02-21_VTChallenge/031-1.pdf"
#
# raw_data <- try(read_results(file), silent = TRUE)
#
# if(any(grep("error", class(raw_data)))){
#   skip("Link to external data is broken")
# } else {
#     # generate standard
#     df_standard_polevault_splits_double <- readRDS(system.file("extdata", "df_standard_polevault_splits_double.rds", package = "JumpeR"))
#
#     # generate test df
#     df_test <- tf_parse(
#       raw_data,
#       flights = TRUE,
#       flight_attempts = TRUE,
#       relay_athletes = TRUE,
#       split_attempts = TRUE
#     )
#
#     # test
#     expect_equivalent(df_standard_polevault_splits_double,
#                       df_test)
#
#     # to regenerate df_standard if df_test is more correct
#     # windows
#     # readr::write_rds(df_test, "~/JumpeR/inst/extdata/df_standard_polevault_splits_double.rds")
#     # mac
#     # readr::write_rds(df_test, "inst/extdata/df_standard_polevault_splits_double.rds")
#     # to compare results
#     # df <- dplyr::anti_join(df_standard_polevault_splits_double, df_test)
#   }
# })

# testthat::test_file("tests/testthat/test-flash_parse_works.R")
