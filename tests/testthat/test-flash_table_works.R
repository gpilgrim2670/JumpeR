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
    df_standard_horizontal <- data.frame(Place = rep("1", 3),
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

test_that("flash table tricky sprint, table rebuild", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2016_Meets/Indoor/02-05_CharlieThomasInvite/001-1-03.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_tricky_sprint <- data.frame(Place = c("1", "2", "3", "4", "5", "6", "FS", "DNS"),
                                         Name = c("Jennifer Madu", "Taylor Williams", "Justise Dayries", "Eboni Sutherland", "Jessica King", "Daresha Petitt", "Torie Robinson", "Sabrina Moore"),
                                         Team = c("Texas A&M", "Clemson", "Baylor", "Rice", "Abilene Christian", "UTSA", "Clemson", "TCU"),
                                         Result = c("7.52", "7.58", "7.64", "7.83", "8.07", "8.25", NA, NA),
                                         Event = rep("60m", 8),
                                         Gender = rep("Women", 8),
                                         stringsAsFactors = FALSE)


    # generate test df
    df_test <- data %>%
      flash_clean_events()


    # test
    expect_equivalent(df_standard_tricky_sprint,
                      df_test)
  }
})

test_that("flash table pole vault", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2018_Meets/Outdoor/04-06_UVAQuad/014-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_PV <- data.frame(Place = c("1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH", "1", "2", "3", "4", "NH"),
                                 Pos = c("4", "3", "2", "1", "5", "4", "3", "2", "1", "5", "4", "3", "2", "1", "5", "4", "3", "2", "1", "5", "4", "3", "2", "1", "5", "4", "3", "2", "1", "5", "4", "3", "2", "1", "5"),
                                 Name = c("Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY", "Katie FREIX", "Chloe BROWN", "Alexandra BUTZ", "Madison MASLOFF", "Bridget GUY" ),
                                 Best = c("3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH"),
                                 Event = rep("Pole Vault", 35),
                                 Gender = rep("Women", 35),
                                 Height = c("3.16m", "3.16m", "3.16m", "3.16m", "3.16m", "3.31m", "3.31m", "3.31m", "3.31m", "3.31m", "3.46m", "3.46m", "3.46m", "3.46m", "3.46m", "3.61m", "3.61m", "3.61m", "3.61m", "3.61m", "3.76m", "3.76m", "3.76m", "3.76m", "3.76m", "3.91m", "3.91m", "3.91m", "3.91m", "3.91m", "4.01m", "4.01m", "4.01m", "4.01m", "4.01m"),
                                 Result = c("---", "O", "XO", "XXO", "---", "---", "O", "XXX", "XXX", "---", "---", "XXX", NA, NA, "---", "O", NA, NA, NA, "---", "O", NA, NA, NA, "---", "O", NA, NA, NA, "---", "XXX", NA, NA, NA, "XXX"),
                                 Team = c("Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia"),
                                 Age = c("JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR"),
                                 stringsAsFactors = FALSE)


    # generate test df
    df_test <- data %>%
      flash_clean_events()


    # test
    expect_equivalent(df_standard_PV,
                      df_test)
  }
})


# testthat::test_file("tests/testthat/test-flash_table_works.R")
