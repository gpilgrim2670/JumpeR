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
                                     Date = rep(as.Date("2019-07-28"), 6),
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

  data <- try(flash_parse_table(file, wide_format = FALSE), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_horizontal <- data.frame(Place = rep("1", 3),
                                         Name = rep("Darrell Hill", 3),
                                         Event = rep("Shot Put", 3),
                                         Gender = rep("Men", 3),
                                         Finals_Result = rep("20.33m", 3),
                                         Date = rep(as.Date("2015-05-30"), 3),
                                         Round = c("1", "2", "3"),
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
                                         Date = rep(as.Date("2016-02-06"), 8),
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
                                 Finals_Result = c("3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH", "3.91m", "3.31m", "3.16m", "3.16m", "NH"),
                                 Event = rep("Pole Vault", 35),
                                 Gender = rep("Women", 35),
                                 Date = rep(as.Date("2018-04-06"), 35),
                                 Height = c("3.16m", "3.16m", "3.16m", "3.16m", "3.16m", "3.31m", "3.31m", "3.31m", "3.31m", "3.31m", "3.46m", "3.46m", "3.46m", "3.46m", "3.46m", "3.61m", "3.61m", "3.61m", "3.61m", "3.61m", "3.76m", "3.76m", "3.76m", "3.76m", "3.76m", "3.91m", "3.91m", "3.91m", "3.91m", "3.91m", "4.01m", "4.01m", "4.01m", "4.01m", "4.01m"),
                                 Result = c("---", "O", "XO", "XXO", "---", "---", "O", "XXX", "XXX", "---", "---", "XXX", NA, NA, "---", "O", NA, NA, NA, "---", "O", NA, NA, NA, "---", "O", NA, NA, NA, "---", "XXX", NA, NA, NA, "XXX"),
                                 Team = c("Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia", "Virginia", "Bucknell", "Bucknell", "Virginia", "Virginia"),
                                 Age = c("JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR", "JR", "SR", "FR", "FR", "JR"),
                                 stringsAsFactors = FALSE) %>%
      dplyr::filter(is.na(Result) == FALSE)


    # generate test df
    df_test <- data %>%
      flash_clean_events()

    # test
    expect_equivalent(df_standard_PV,
                      df_test)
  }
})

test_that("flash table distance", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2017_Meets/Outdoor/04-29_VirginiaGrandPrix/025-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_distance <- data.frame(
      Place = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "DNF", "DNS", "DNS", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "DNF", "DNS", "DNS", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "DNF", "DNS", "DNS", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "DNF", "DNS", "DNS"),
      Lane = c("1", "9", "4", "5", "15", "11", "13", "8", "14", "10", "3", "12", "2", "6", "17", "7", "16", "1", "9", "4", "5", "15", "11", "13", "8", "14", "10", "3", "12", "2", "6", "17", "7", "16", "1", "9", "4", "5", "15", "11", "13", "8", "14", "10", "3", "12", "2", "6", "17", "7", "16", "1", "9", "4", "5", "15", "11", "13", "8", "14", "10", "3", "12", "2", "6", "17", "7", "16"),
      Name = c("Khalil RMIDI KININI", "Colin SCHULTZ", "Andrew GOLDMAN", "Lachlan COOK", "Jack JOYCE", "Robby CREESE", "Michael TAMAYO", "Matt BOMKAMP", "AJ ERNST", "Robby KEOUGH", "Justin MOAKLER", "Randy NEISH", "Dylan RICH", "Cameron FRANCIS", "Jayquan WILLIAMS", "Philip HALL", "Patrick MCGREGOR", "Khalil RMIDI KININI", "Colin SCHULTZ", "Andrew GOLDMAN", "Lachlan COOK", "Jack JOYCE", "Robby CREESE", "Michael TAMAYO", "Matt BOMKAMP", "AJ ERNST", "Robby KEOUGH", "Justin MOAKLER", "Randy NEISH", "Dylan RICH", "Cameron FRANCIS", "Jayquan WILLIAMS", "Philip HALL", "Patrick MCGREGOR", "Khalil RMIDI KININI", "Colin SCHULTZ", "Andrew GOLDMAN", "Lachlan COOK", "Jack JOYCE", "Robby CREESE", "Michael TAMAYO", "Matt BOMKAMP", "AJ ERNST", "Robby KEOUGH", "Justin MOAKLER", "Randy NEISH", "Dylan RICH", "Cameron FRANCIS", "Jayquan WILLIAMS", "Philip HALL", "Patrick MCGREGOR", "Khalil RMIDI KININI", "Colin SCHULTZ", "Andrew GOLDMAN", "Lachlan COOK", "Jack JOYCE", "Robby CREESE", "Michael TAMAYO", "Matt BOMKAMP", "AJ ERNST", "Robby KEOUGH", "Justin MOAKLER", "Randy NEISH", "Dylan RICH", "Cameron FRANCIS", "Jayquan WILLIAMS", "Philip HALL", "Patrick MCGREGOR"),
      Age = c("SR", NA,  "SO", "FR", "FR", NA,  NA,  "FR", "FR", "FR", "SO", "FR", "SR", "JR", "FR", "SO", NA,  "SR", NA,  "SO", "FR", "FR", NA,   NA,  "FR", "FR", "FR", "SO", "FR", "SR", "JR", "FR", "SO", NA,   "SR", NA,  "SO", "FR", "FR", NA,   NA,   "FR", "FR", "FR", "SO", "FR", "SR", "JR",  "FR", "SO", NA,   "SR", NA,   "SO", "FR", "FR", NA,   NA,   "FR", "FR", "FR", "SO", "FR", "SR", "JR", "FR", "SO", NA),
      Team = c("UMES", "Unattached", "Virginia Tech", "Virginia", "Virginia Tech", "USA", "Run For Your Life TC", "Charlotte", "Virginia", "Virginia", "Morehead State", "Virginia", "USA", "Liberty", "Charlotte", "North Carolina St.", "USA", "UMES", "Unattached", "Virginia Tech", "Virginia", "Virginia Tech", "USA", "Run For Your Life TC", "Charlotte", "Virginia", "Virginia", "Morehead State", "Virginia", "USA", "Liberty", "Charlotte", "North Carolina St.", "USA", "UMES", "Unattached", "Virginia Tech", "Virginia", "Virginia Tech", "USA", "Run For Your Life TC", "Charlotte", "Virginia", "Virginia", "Morehead State", "Virginia", "USA", "Liberty", "Charlotte", "North Carolina St.", "USA", "UMES", "Unattached", "Virginia Tech", "Virginia", "Virginia Tech", "USA", "Run For Your Life TC", "Charlotte", "Virginia", "Virginia", "Morehead State", "Virginia", "USA", "Liberty", "Charlotte", "North Carolina St.", "USA"),
      Result = c("3:46.70", "3:47.14", "3:47.33", "3:47.53", "3:47.79", "3:48.45", "3:49.70", "3:50.10", "3:50.88", "3:51.76", "3:53.52", "3:54.54", "3:58.31", "4:20.37", NA, NA, NA, "3:46.70", "3:47.14", "3:47.33", "3:47.53", "3:47.79", "3:48.45", "3:49.70", "3:50.10", "3:50.88", "3:51.76", "3:53.52", "3:54.54", "3:58.31", "4:20.37", NA, NA, NA, "3:46.70", "3:47.14", "3:47.33", "3:47.53", "3:47.79", "3:48.45", "3:49.70", "3:50.10", "3:50.88", "3:51.76", "3:53.52", "3:54.54", "3:58.31", "4:20.37", NA, NA, NA, "3:46.70", "3:47.14", "3:47.33", "3:47.53", "3:47.79", "3:48.45", "3:49.70", "3:50.10", "3:50.88", "3:51.76", "3:53.52", "3:54.54", "3:58.31", "4:20.37", NA, NA, NA),
      Event = rep("1500m", 68),
      Gender = rep("Men", 68),
      Date = rep(as.Date("2017-04-29"), 68),
      Split_Distance = c(rep("300", 17), rep("700", 17), rep("1100", 17), rep("1500", 17)),
      Split_Time = c("44.81", "44.40", "44.70", "44.16", "44.96", "45.28", "44.03", "45.19", "44.51", "45.06", "45.49", "44.29", "45.73", "46.00", "43.92", NA, NA, "1:46.73", "1:46.11", "1:46.53", "1:45.85", "1:46.71", "1:46.32", "1:45.75", "1:46.94", "1:46.62", "1:47.00", "1:47.25", "1:46.09", "1:48.16", "1:53.18", "1:45.58", NA, NA, "2:49.10", "2:48.28", "2:48.36", "2:48.23", "2:49.43", "2:48.88", "2:48.06", "2:49.62", "2:51.65", "2:49.98", "2:51.14", "2:49.41", "2:53.86", "3:06.30", NA, NA, NA, "3:46.70", "3:47.14", "3:47.33", "3:47.53", "3:47.79", "3:48.45", "3:49.70", "3:50.10", "3:50.88", "3:51.76", "3:53.52", "3:54.54", "3:58.31", "4:20.37", NA, NA, NA),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events()

    # test
    expect_equivalent(df_standard_distance,
                      df_test)
  }
})

test_that("flash table distance medley", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2015_Meets/Indoor/03-13_NCAA/026-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_distance_medley <- data.frame(
      Place = rep("1", 4),
      # Name = rep("Arkansas", 4),
      Team = rep("Arkansas", 4),
      Result = rep("10:51.89", 4),
      Event = rep("Dmr", 4),
      Gender = rep("Women", 4),
      Date = rep(as.Date("2015-03-15"), 4),
      Split_Distance = c("L1", "L2", "L3", "L4"),
      Split_Time = c("3:22.87", "4:15.71", "6:23.00", "10:51.89"),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      filter(Place == 1)

    # test
    expect_equivalent(df_standard_distance_medley,
                      df_test)
  }
})

test_that("flash table Pent 800m", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/034-5_compiled.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_pent <- data.frame(
      Place = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"),
      Name = c("Halley FOLSOM", "Sterling LESTER", "Jadin O'BRIEN", "Anna HALL", "Erin MARSH", "Kristine BLAZEVICA", "Mathilde REY", "Alix STILL", "Anna BUSH", "Jordan HIRSBRUNNER", "Annika WILLIAMS", "Nicola ADER", "Shayla BROUGHTON", "Allison GERADS", "Tyra GITTENS", "G'Auna EDWARDS"),
      Result = c("2:10.86", "2:12.13", "2:13.13", "2:13.19", "2:17.53", "2:18.59", "2:18.63", "2:21.70", "2:22.65", "2:22.90", "2:24.59", "2:25.95", "2:26.37", "2:27.20", "2:28.22", "2:29.24"),
      Points = c("952", "934", "919", "918", "857", "843", "842", "801", "788", "785", "762", "745", "739", "728", "715", "702"),
      Event = rep("Pent 800m", 16),
      Gender = rep("Women", 16),
      Date = rep(as.Date("2021-03-12"), 16),
      Team = c("BYU", "Florida", "Notre Dame", "Georgia", "Duke", "Texas", "Oregon", "Virginia", "Wake Forest", "Wisconsin", "Kentucky", "Nevada", "Miss State", "Minnesota", "Texas A&M", "Arkansas"),
      Age = c("SR", "JR", "FR", "SO", "SR", "FR", "SO", "JR", "SO", "SO", "SO", "SR", "SR", "JR", "JR", "JR"),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      select(-`Sec..pl.`)

    # test
    expect_equivalent(df_standard_pent,
                      df_test)
  }
})

test_that("flash table 400m tiebreaker", {

  # these results should contain a tiebreaker column

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2015_Meets/Indoor/01-09_TamuHS/023-2_compiled.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_tiebreaker <- data.frame(
      Place = c("1", "2", "3"),
      Name = c("Howard Fields", "Drake Murphy", "Desmond Jefferson"),
      Team = c("Wings", "Mainland Jaguars", "A Running Start TC"),
      Result = c("48.50", "48.50", "49.09"),
      Event = rep("400m", 3),
      Gender = rep("Boys", 3),
      Date = rep(as.Date("2015-01-10"), 3),
      Tiebreaker = c("48.491", "48.496", NA),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      select(-`Heat..Pl.`) %>%
      filter(as.numeric(Place) <= 3)

    # test
    expect_equivalent(df_standard_tiebreaker,
                      df_test)
  }
})

test_that("flash table long jump, wind without w: marker", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2021_Meets/Outdoor/04-16_VirginiaChallenge/035-1_compiledSeries.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard_wind <- data.frame(
      Place = c("5", "12", "13"),
      Name = c("Ezra MELLINGER", "Evan LEE", "Harry LORD" ),
      Event = rep("Long Jump", 3),
      Gender = rep("Men", 3),
      Best = c("7.30m", "6.71m", "6.55m"),
      Date = rep(as.Date("2021-04-17"), 3),
      Round = rep("1", 3),
      Result = c("7.02", "6.57", "6.36"),
      Team = rep("Duke", 3),
      Wind = c("-0.7", "+0.1", "+1.3"),
      Age = c("FR", "SR", "JR"),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      filter(Team == "Duke") %>%
      head(3)

    # test
    expect_equivalent(df_standard_wind,
                      df_test)
  }
})

test_that("flash table DMR, several record strings", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2021_Meets/Indoor/02-25_SEC/030-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard <- data.frame(
      Place = rep("1", 4),
      Pos = rep("9", 4),
      Team = rep("OLE MISS", 4),
      Result = rep("9:29.35", 4),
      Event = rep("Dmr", 4),
      Gender = rep("Men", 4),
      Event_Date = rep(as.Date("2021-02-27"), 4),
      Split_Distance = c("L1", "L2", "L3", "L4"),
      Split_Time = c("2:57.08", "3:45.60", "5:34.67", "9:29.35"),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events() %>%
      filter(Team == "OLE MISS")

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("flash table sprint with splits, keep wide format", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2021_Meets/Indoor/03-11_NCAA/017-2-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard <- data.frame(
      Place = as.character(seq(1, 8, 1)),
      Pos = as.character(c(3, 4, 5, 7, 8, 6, 2, 1)),
      Name = c("Kemba NELSON", "Twanisha TERRY", "Kiara GRANT", "Tamara CLARK", "Alfreda STEELE", "Joella LLOYD", "Jada BAYLARK", "Halle HAZZARD"),
      Result = c("7.05", "7.14", "7.16", "7.18", "7.22", "7.23", "7.23", "7.27"),
      Reaction_Time = c("0.179", "0.153", "0.168", "0.179", "0.219", "0.216", "0.172", "0.151"),
      Split_30 = c("4.13", "4.17", "4.16", "4.21", "4.28", "4.23", "4.22", "4.24"),
      Split_60 = c("7.05", "7.14", "7.16", "7.18", "7.22", "7.23", "7.23", "7.27"),
      Event = rep("60m", 8),
      Gender = rep("Women", 8),
      Event_Date = rep(as.Date("2021-03-14"), 8),
      Team = c("Oregon", "USC", "Norfolk State", "Alabama", "Miami (Fla.)", "Tennessee", "Arkansas", "Virginia"),
      Age = c("JR", "SR", "JR", "SR", "SR", "SO", "SR", "SR"),
      stringsAsFactors = FALSE
    )

    # generate test df
    df_test <- data %>%
      flash_clean_events(wide_format_clean = TRUE)

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("flash table relay with splits", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2018_Meets/Outdoor/04-13_JohnMcDonnell/012-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard <-
      structure(
        list(
          Place = c("1", "2", "1", "2", "1", "2"),
          Lane = c("6",
                   "5", "6", "5", "6", "5"),
          Team = c(
            "OKLAHOMA STATE",
            "OKLAHOMA STATE \"B\"",
            "OKLAHOMA STATE",
            "OKLAHOMA STATE \"B\"",
            "OKLAHOMA STATE",
            "OKLAHOMA STATE \"B\""
          ),
          Result = c(
            "3:55.36",
            "3:55.38",
            "3:55.36",
            "3:55.38",
            "3:55.36",
            "3:55.38"
          ),
          Event = c(
            "4x400m Relay",
            "4x400m Relay",
            "4x400m Relay",
            "4x400m Relay",
            "4x400m Relay",
            "4x400m Relay"
          ),
          Gender = c("Women",
                     "Women", "Women", "Women", "Women", "Women"),
          Event_Date = structure(c(17635,
                                   17635, 17635, 17635, 17635, 17635), class = "Date"),
          Split_Distance = c("L1...L2",
                             "L1...L2", "L3", "L3", "L4", "L4"),
          Split_Time = c(
            "1:58.29",
            "1:57.18",
            "2:54.14",
            "2:54.05",
            "3:55.36",
            "3:55.38"
          )
        ),
        row.names = c(NA,-6L),
        class = "data.frame"
      )
    # generate test df
    df_test <- data %>%
      flash_clean_events()

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("4x110m Hurdles Shuttle", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2018_Meets/Outdoor/06-15_NBHSON/045-1_compiled.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard <-
      structure(
        list(
          Place = c(
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9",
            "10",
            "11",
            "12",
            "13",
            "14",
            "15",
            "16",
            "17",
            "18",
            "19",
            "20",
            "21",
            "22",
            "23",
            "24",
            NA,
            NA,
            NA,
            NA,
            NA,
            NA
          ),
          Team = c(
            "EAST ORANGE TC-NJ",
            "WESTERN BRANCH TC-VA",
            "KENTWOOD TC-MI",
            "RAHWAY TC-NJ",
            "SKY'S THE LIMIT TC-NY",
            "VIPER TC-NJ",
            "RED RAIDER TC-NY",
            "GOLDEN EAGLES RUNNING-NJ",
            "KEARNY TC-NJ",
            "WOBURN TC-MA",
            "HIGHLAND SPRINGS TC-VA",
            "FRANKLIN TC-NJ",
            "UNION CATHOLIC TC-NJ",
            "RIDGEWOOD TC-NJ",
            "RAIDERS TC-NJ",
            "LAKELAND/PANAS TC-NY",
            "BLUE KNIGHTS TC-NJ",
            "LINDEN TC-NJ",
            "BOBCATS TC-VA",
            "DELBARTON TC-NJ",
            "SETON HALL TC-NJ",
            "TOP OF THE HILL TC-NJ",
            "ST AUGUSTINE PREP HERMITS-NJ",
            "HAMILTON TC-NJ",
            "WINSLOW TOWNSHIP TC-NJ",
            "BORO TC-NJ",
            "SPRINGBORO TC-OH",
            "WESTERN BRANCH TC-VA",
            "MARBLEHEAD TC-MA",
            "WESTSIDE TC-NJ"
          ),
          Result = c(
            "56.86",
            "57.78",
            "58.45",
            "58.91",
            "59.96",
            "1:00.11",
            "1:00.20",
            "1:00.72",
            "1:00.98",
            "1:01.26",
            "1:01.35",
            "1:01.63",
            "1:01.98",
            "1:02.26",
            "1:02.67",
            "1:02.77",
            "1:02.80",
            "1:03.05",
            "1:03.06",
            "1:03.41",
            "1:04.21",
            "1:04.33",
            "1:05.47",
            "1:07.13",
            "DQ",
            "DQ",
            "DQ",
            "DQ",
            "DQ",
            "DNS"
          ),
          Wind = c(
            "+0.9",
            "+0.9",
            "+0.9",
            "+0.9",
            "+1.4",
            "+1.4",
            "+0.7",
            "+0.6",
            "+0.6",
            "+1.4",
            "+0.7",
            "+1.0",
            "+0.7",
            "+1.9",
            "+2.4",
            "+1.0",
            "+1.9",
            "+0.7",
            "+2.4",
            "+1.9",
            "+2.4",
            "+1.1",
            "+1.1",
            "+2.4",
            NA,
            NA,
            NA,
            NA,
            NA,
            NA
          ),
          Sec..pl. = c(
            "8 (1)",
            "8 (2)",
            "8 (3)",
            "8 (4)",
            "7 (1)",
            "7 (2)",
            "5 (1)",
            "6 (1)",
            "6 (2)",
            "7 (3)",
            "5 (2)",
            "2 (1)",
            "5 (3)",
            "3 (1)",
            "4 (1)",
            "2 (2)",
            "3 (2)",
            "5 (4)",
            "4 (2)",
            "3 (3)",
            "4 (3)",
            "1 (1)",
            "1 (2)",
            "4 (4)",
            "2",
            "2",
            "3",
            "7",
            "1",
            "6"
          ),
          Event = c(
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle",
            "4x110m Shuttle Hurdle"
          ),
          Gender = c(
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys",
            "Boys"
          ),
          Event_Date = structure(
            c(
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698,
              17698
            ),
            class = "Date"
          )
        ),
        row.names = c(NA,-30L),
        class = "data.frame"
      )
    # generate test df
    df_test <- data %>%
      flash_clean_events()

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("Women 1600 SMR", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://flashresults.com/2018_Meets/Outdoor/04-27_NRChampionships/049-1-01.htm"

  data <- try(flash_parse_table(file), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
    df_standard <-
      structure(
        list(
          Place = c("1", "2", "3", "4", "5", "1", "2", "3",
                    "4", "5"),
          Lane = c("5", "7", "6", "4", "8", "5", "7", "6", "4",
                   "8"),
          Team = c(
            "STANFORD",
            "BAYLOR",
            "FLORIDA",
            "KENTUCKY",
            "MIAMI",
            "STANFORD",
            "BAYLOR",
            "FLORIDA",
            "KENTUCKY",
            "MIAMI"
          ),
          Result = c(
            "3:41.59",
            "3:42.11",
            "3:47.48",
            "3:47.93",
            "3:52.70",
            "3:41.59",
            "3:42.11",
            "3:47.48",
            "3:47.93",
            "3:52.70"
          ),
          Event = c(
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr",
            "1600 Smr"
          ),
          Gender = c(
            "Women",
            "Women",
            "Women",
            "Women",
            "Women",
            "Women",
            "Women",
            "Women",
            "Women",
            "Women"
          ),
          Event_Date = structure(
            c(
              17649,
              17649,
              17649,
              17649,
              17649,
              17649,
              17649,
              17649,
              17649,
              17649
            ),
            class = "Date"
          ),
          Split_Distance = c(
            "L1.L3",
            "L1.L3",
            "L1.L3",
            "L1.L3",
            "L1.L3",
            "800",
            "800",
            "800",
            "800",
            "800"
          ),
          Split_Time = c(
            "1:40.70",
            "1:40.41",
            "1:40.16",
            "1:39.84",
            "1:40.53",
            "3:41.59",
            "3:42.11",
            "3:47.48",
            "3:47.93",
            "3:52.70"
          )
        ),
        row.names = c(NA,-10L),
        class = "data.frame"
      )
    # generate test df
    df_test <- data %>%
      flash_clean_events()

    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})

test_that("flash table empty columns", {

  skip_on_cran() # due to risk of external resources failing

  file <-
    "https://www.flashresults.com/2019_Meets/Outdoor/06-05_NCAAOTF-Austin/015-1_compiled.htm"

  df_test <- try(flash_parse_table(file, clean = TRUE), silent = TRUE)

  if (any(grep("error", class(data)))) {
    skip("Link to external data is broken")
  } else {

    # build standard
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


    # test
    expect_equivalent(df_standard,
                      df_test)
  }
})


# testthat::test_file("tests/testthat/test-flash_table.R")
