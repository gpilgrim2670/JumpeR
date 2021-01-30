test_that("flash parse works sprint", {
  file <- "https://www.flashresults.com/2019_Meets/Outdoor/05-09_SEC/029-1.pdf"
  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
  } else {
    df_standard <- data.frame(Place = as.character(seq(1, 15, 1)),
                              Name = c("Grant HOLLOWAY", "Daniel ROBERTS", "Isaiah MOORE", "Damion THOMAS", "Tai BROWN", "Michael NICHOLLS", "Shakiel CHATTOO", "Carl ELLIOTT III", "Robert DUNNING", "Caleb WILT", "Arthur PRICE", "Cory POOLE", "Kenney BROADNAX", "Tre'Bien GILBERT", "Craig CLARK"),
                              Age = c( "JR", "JR", "SR", "SO", "FR", "SR", "SR", NA, "JR", "SR", "SR", "SO", "FR", "FR", "SO"),
                              Team = c("FLORIDA", "KENTUCKY", "SOUTH CAROLINA", "LSU", "KENTUCKY", "GEORGIA", "ARKANSAS", "ARKANSAS", "ALABAMA", "KENTUCKY", "LSU", "FLORIDA", "OLE MISS", "ARKANSAS", "AUBURN"),
                              Finals_Result = c("13.07", "13.31", "13.49", "13.57", "13.62", "13.64", "13.79", "13.79", "13.83", "13.85", "13.89", "13.91", "13.99", "14.30", "14.36"),
                              Wind_Speed = c("+0.8", "+0.8", "+0.9", "+0.9", "+0.8", "+0.8", "+0.9", "+0.9", "+0.8", "+0.9", "+0.9", "+0.8", "+0.9", "+0.8", "+0.8"),
                              Notes = c(NA, NA, NA, NA, NA, NA, "13.784", "13.786", NA, NA, NA, NA, NA, NA , NA),
                              DQ = rep(0, 15),
                              Event = rep("Men 110 M Hurdles Prelims 6:00 PM 10 May 2019", 15))

    df_test <- tf_parse(read_results(file), attempts = TRUE, attempts_results = TRUE, relay_athletes = TRUE)

    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("flash parse works long jump, team scores in results", {
  file <- "https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/015-1.pdf"
  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
  } else {

    # import standard
    df_standard_longjump <- readRDS(system.file("extdata", "df_standard_longjump.rds", package = "JumpeR"))

    df_test <- tf_parse(read_results(file), attempts = TRUE, attempts_results = TRUE, relay_athletes = TRUE)

    expect_equivalent(df_standard_longjump,
                      df_test)
  }

})


# test_that("flash parse works relay, team scores in results", {
#   file <- "https://www.flashresults.com/2018_Meets/Outdoor/05-05_A10/032-1.pdf"
#   if (is_link_broken(file) == TRUE) {
#     warning("Link to external data is broken")
#   } else {
#     df_standard <- data.frame(Place = as.character(seq(1, 15, 1)),
#                               Name = c("Grant HOLLOWAY", "Daniel ROBERTS", "Isaiah MOORE", "Damion THOMAS", "Tai BROWN", "Michael NICHOLLS", "Shakiel CHATTOO", "Carl ELLIOTT III", "Robert DUNNING", "Caleb WILT", "Arthur PRICE", "Cory POOLE", "Kenney BROADNAX", "Tre'Bien GILBERT", "Craig CLARK"),
#                               Age = c( "JR", "JR", "SR", "SO", "FR", "SR", "SR", NA, "JR", "SR", "SR", "SO", "FR", "FR", "SO"),
#                               Team = c("FLORIDA", "KENTUCKY", "SOUTH CAROLINA", "LSU", "KENTUCKY", "GEORGIA", "ARKANSAS", "ARKANSAS", "ALABAMA", "KENTUCKY", "LSU", "FLORIDA", "OLE MISS", "ARKANSAS", "AUBURN"),
#                               Finals_Result = c("13.07", "13.31", "13.49", "13.57", "13.62", "13.64", "13.79", "13.79", "13.83", "13.85", "13.89", "13.91", "13.99", "14.30", "14.36"),
#                               Wind_Speed = c("+0.8", "+0.8", "+0.9", "+0.9", "+0.8", "+0.8", "+0.9", "+0.9", "+0.8", "+0.9", "+0.9", "+0.8", "+0.9", "+0.8", "+0.8"),
#                               Notes = c(NA, NA, NA, NA, NA, NA, "13.784", "13.786", NA, NA, NA, NA, NA, NA , NA),
#                               DQ = rep(0, 15),
#                               Event = rep("Men 110 M Hurdles Prelims 6:00 PM 10 May 2019", 15))
#
#     df_test <- tf_parse(read_results(file), attempts = TRUE, attempts_results = TRUE, relay_athletes = TRUE)
#
#     expect_equivalent(df_standard,
#                       df_test)
#   }
# })


# testthat::test_file("tests/testthat/test-flash_parse_works.R")
