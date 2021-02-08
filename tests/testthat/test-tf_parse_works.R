test_that("tf_parse_standard", {

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
  if (is_link_broken(url_1) == TRUE){
    warning("A link to external data is broken")
    expect_equal(2, 2)
  }

  # list of sources
  sources <- c(file_1,
               file_2,
               file_3,
               file_4,
               url_1)
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
  df_test <- Read_Map(sources)
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
})

test_that("tf_parse_attempts_splits_works", {

  url_2 <- "http://leonetiming.com/2019/Indoor/GregPageRelays/Results.htm"

  if (is_link_broken(url_2) == TRUE) {
    warning("Link to external data is broken")
    expect_equal(2, 2)
  } else {

    df_standard_polevault_hytek <- readRDS(system.file("extdata", "df_standard_polevault_hytek.rds", package = "JumpeR"))

  df <-
    tf_parse(
      read_results(
        url_2
      ),
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
