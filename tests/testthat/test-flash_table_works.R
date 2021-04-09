test_that("flash table sprint works", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2019_Meets/Outdoor/07-25_USATF_CIS/004-1-03.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_sprint <- data.frame(Place = c("1", "2", "3", "4", "5", "DNS"),
                              Lane = c("7", "4", "8", "5", "6", "3"),
                              Name = c("Kenny BEDNAREK", "Marqueze WASHINGTON", "McKinely WEST", "Courtney LINDSEY", "Cameron PARKER", "Kendal WILLIAMS"),
                              Team = c("NIKE", "Unattached", "Unattached", "Iowa Central Community College", "Unattached", "adidas"),
                              Result = c("20.85", "21.04", "21.20", "21.32", "21.92", NA),
                              Reaction_time = c("0.191", "0.189", "0.178", "0.185", "0.220", NA),
                              Event = rep("200m", 6),
                              Gender = rep("Men", 6),
                              Wind = rep("-1.1", 6),
                              stringsAsFactors = FALSE)


    # generate test df
    df_test <- data %>%
      flash_clean_events()


    # test
    expect_equivalent(df_standard_sprint,
                      df_test)
  }
})

test_that("flash table horizontal works", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2015_Meets/Outdoor/05-28_NCAAEast/017-1_compiledSeries.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_horizontal <- data.frame(Place = rep(1, 3),
                                     Name = rep("Darrell Hill", 3),
                                     Event = rep("Shot Put", 3),
                                     Gender = rep("Men", 3),
                                     Round = c(1, 2, 3),
                                     Result = c("18.06", "20.33", "19.51"),
                                     Flight = rep("4", 3),
                                     Team = rep("Penn State", 3),
                                     Age = rep("SR", 3),
                                     stringsAsFactors = FALSE)


    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      filter(Name == "Darrell Hill")


    # test
    expect_equivalent(df_standard_horizontal,
                      df_test)
  }
})

# testthat::test_file("tests/testthat/test-flash_table_works.R")
