test_that("hytek splits 1", {

  skip_on_cran()

  file <- "http://results.deltatiming.com/ncaa/tf/2019-joe-walker-invitational/print/190412F010"

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
                             "20", "21", "22", NA, NA, NA, NA, NA, NA), Name = c("Suliman, Waleed",
                                                                                 "Bethmann, Cade", "Comber, Casey", "Hengst, Dalton", "Garcia Romo, Mario",
                                                                                 "Smulders, Everett", "Bartlett, Avery", "Curles, Wesley", "Mueller, Lance",
                                                                                 "Skelly, Landon", "Waters, Seth", "Kielhofner, Stephen", "Garriques, Brennan",
                                                                                 "Oury, Jacob", "Galimah, Jallah", "Perkins, Austin", "Penney, Thomas",
                                                                                 "Reed, Curtis", "langat, Vincent", "Dolan, Nick", "Agbede, Toshin",
                                                                                 "Holbrook, Patrick", "Gutierrez, Derek", "Scott, Parker", "Kiplagat, Henry",
                                                                                 "White, Trevor", "Jones, Colin", "Vaughn, Blake"), Team = c("Ole Miss",
                                                                                                                                             "Ole Miss", "Villanova", "Ole Miss", "Ole Miss", "Ole Miss",
                                                                                                                                             "Georgia Tech", "Unattached", "Saint Louis", "SIU Edwardsville",
                                                                                                                                             "Arkansas State", "Saint Louis", "Troy", "Arkansas State", "Arkansas State",
                                                                                                                                             "SE Missouri", "Saint Louis", "SE Missouri", "Jackson State",
                                                                                                                                             "Saint Louis", "Arkansas-Pine Bluff", "Saint Louis", "Ole Miss",
                                                                                                                                             "Unattached", "Jackson State", "McKendree", "SE Missouri", "Troy"
                                                                                 ), Finals_Result = c("3:40.49", "3:41.65", "3:42.54", "3:43.92",
                                                                                                      "3:44.69", "3:44.94", "3:45.08", "3:49.77", "3:54.88", "3:55.06",
                                                                                                      "3:58.41", "3:59.45", "3:59.85", "4:01.73", "4:04.01", "4:04.15",
                                                                                                      "4:04.37", "4:04.88", "4:05.46", "4:06.56", "4:12.24", "4:12.51",
                                                                                                      "DNF", "DNF", "DNS", "DNS", "DNS", "DNS"), DQ = c(0, 0, 0, 0,
                                                                                                                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
                                                                                                                                                        0, 0, 0), Event = c("Men 1500 M", "Men 1500 M", "Men 1500 M",
                                                                                                                                                                            "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M",
                                                                                                                                                                            "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M",
                                                                                                                                                                            "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M",
                                                                                                                                                                            "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M",
                                                                                                                                                                            "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M", "Men 1500 M"
                                                                                                                                                        ), Split_1 = c("43.992", "44.116", "45.116", "44.665", "44.271",
                                                                                                                                                                       "44.868", "44.494", "45.291", NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                       NA, NA, NA, NA, NA, NA, "43.595", "43.801", NA, NA, NA, NA),
                   Split_2 = c("1:01.757", "1:01.823", "1:00.741", "1:01.630",
                               "1:01.832", "1:01.600", "1:01.913", "1:01.672", NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1:01.651", "1:01.693",
                               NA, NA, NA, NA), Split_3 = c("59.557", "59.530", "59.647",
                                                            "59.689", "59.653", "59.716", "59.473", "1:00.298", NA, NA,
                                                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1:01.144",
                                                            NA, NA, NA, NA), Split_4 = c("55.179", "56.176", "57.028",
                                                                                         "57.934", "58.925", "58.753", "59.197", "1:02.501", NA, NA,
                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                         NA, NA, NA)), row.names = c(NA, -28L), class = "data.frame")

  expect_equivalent(df_test, df_standard)

})

test_that("hytek splits 2", {

  skip_on_cran()

  file <- "http://results.deltatiming.com/ncaa/tf/2019-joe-walker-invitational/190412F009"

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
                             "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                             "31", "32", NA, NA), Name = c("King, Madeleine", "Ruiz, Amy",
                                                           "Fegans, Nicole", "Traxler, Ylva", "Galarza, Liz", "Elkin, Anna",
                                                           "Herndon, Hana", "Gallagher, Annie", "Knott, Carli", "Shea, Kaitlyn",
                                                           "Rose, Morgan Claire", "Link, Courtney", "Mohrmann, Danielle",
                                                           "Meyer, Pauline", "Goff, Aly", "Walsh, Macki", "Hutchcraft, Rachel",
                                                           "Keller, Maddie", "Anderson, Katie", "Thomas, Anna", "Guirey, Krystin",
                                                           "Ellis, Emily", "Daley, Kiera", "Hayes, Taylor", "Riley, Mary",
                                                           "Wooten, Sabria", "Ng, Megan", "Langevin, Anna", "Bethea-Nurse, Ajani",
                                                           "Quinonez, Tatyana", "Wood, Gabby", "Chebet, Mercy", "McHugh, Maddie",
                                                           "Brown, Shelby"), Team = c("Ole Miss", "Georgia Tech", "Georgia Tech",
                                                                                      "Ole Miss", "Georgia Tech", "Ole Miss", "Georgia Tech", "Georgia Tech",
                                                                                      "SE Missouri", "SE Missouri", "Ole Miss", "Saint Louis", "SE Missouri",
                                                                                      "Arkansas State", "SIU Edwardsville", "Troy", "SE Missouri",
                                                                                      "SE Missouri", "Saint Louis", "SE Missouri", "Troy", "SIU Edwardsville",
                                                                                      "Saint Louis", "Jackson State", "Saint Louis", "Alabama A&M",
                                                                                      "Saint Louis", "Saint Louis", "Arkansas-Pine Bluff", "Jackson State",
                                                                                      "SIU Edwardsville", "Jackson State", "Unattached", "Ole Miss"
                                                           ), Finals_Result = c("4:21.51", "4:22.65", "4:22.96", "4:26.04",
                                                                                "4:27.17", "4:27.31", "4:27.59", "4:27.69", "4:27.99", "4:38.20",
                                                                                "4:41.43", "4:43.29", "4:44.16", "4:44.45", "4:44.81", "4:46.91",
                                                                                "4:48.82", "4:49.69", "4:51.32", "4:54.46", "4:56.52", "4:58.14",
                                                                                "4:59.55", "5:00.45", "5:00.64", "5:02.74", "5:04.79", "5:04.88",
                                                                                "5:07.38", "5:13.14", "5:16.46", "5:21.69", "DNF", "DNF"), DQ = c(0,
                                                                                                                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                                                                                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1), Event = c("Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M", "Women 1500 M", "Women 1500 M", "Women 1500 M",
                                                                                                                                                                                                 "Women 1500 M"), Split_1 = c("2:02.952", "2:03.487", "2:03.196",
                                                                                                                                                                                                                              "2:03.753", "2:03.887", "2:04.433", "2:04.079", "2:03.603", "2:04.226",
                                                                                                                                                                                                                              "2:04.920", NA, NA, NA, "2:08.343", NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Split_2 = c("1:12.231",
                                                                                                                                                                                                                                                                                               "1:12.163", "1:12.150", "1:12.269", "1:12.427", "1:12.694", "1:12.528",
                                                                                                                                                                                                                                                                                               "1:12.442", "1:12.614", "1:16.073", NA, NA, NA, "1:20.343", NA,
                                                                                                                                                                                                                                                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                               NA, NA, NA), Split_3 = c("1:06.326", "1:06.994", "1:07.610",
                                                                                                                                                                                                                                                                                                                        "1:10.018", "1:10.850", "1:10.177", "1:10.978", "1:11.640", "1:11.150",
                                                                                                                                                                                                                                                                                                                        "1:17.202", NA, NA, NA, "1:15.756", NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                            -34L), class = "data.frame")

  expect_equivalent(df_test, df_standard)

})
