#' Reads track and field results into a list of strings in preparation for parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove
#'
#' @param file a .pdf or .html file (could be a url) where containing track and field results.  Must be formatted in a "normal" fashion - see vignette
#' @param avoid xxx
#' @param typo xxx
#' @param replacement xxx
#'
#' @return a dataframe
#'
#' @seealso \code{tf_parse} is meant to be preceeded by \code{\link{read_results}}
#'
#' @export

tf_parse <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default) {


    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    if(length(typo) != length(replacement)) {
      stop("typo and replacement must have the same number of elements (be the same length)")
    }

    #### strings that if a line begins with one of them the line is ignored ####
    avoid_default <-
      c("Record\\:")

    #### define avoid_minimal ####
    avoid_minimal <- c("^\\s{1,}r\\:")

    #### testing setup ####
    file_1 <-
       system.file("extdata", "Results-IVP-Track-Field-Championship-2019-20-v2.pdf", package = "JumpeR")

    file_2 <- "http://results.yentiming.com/2019/Indoor/12-21-18-west.htm"

    file_1 <- read_results(file_1)
    file_2 <- read_results(file_2)
    file <- c(file_1, file_2)
    avoid <- avoid_default
    typo <- typo_default
    replacement <- replacement_default

    #### assign row numbers ####
    as_lines_list_2 <- add_row_numbers(text = file)

      #### Pulls out event labels from text ####
    events <- event_parse(as_lines_list_2) %>%
      dplyr::mutate(Event = stringr::str_remove(Event, " Women$| Men$"))


      #### set up strings ####
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Result_String <- "\\d{0,2}\\:?\\-?\\d{1,2}\\.\\d{2}m?"
      Result_Specials_String <- paste0(Result_String, "|^NT$|^NP$|^DQ$|^DNS$|^DNF$|^FOUL$")
      Wind_String <- "\\+\\d\\.\\d|\\-\\d\\.\\d|^NWS$|^NWI$|^\\d\\.\\d$"
      Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"

      #### clean input data ####
      suppressWarnings(
        data_1 <- as_lines_list_2 %>%
          stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") %>%  # removes * placed in front of place number in ties
          .[purrr::map(., length) > 0] %>%
          .[purrr::map(., stringr::str_length) > 50] %>%
          .[purrr::map_lgl(., stringr::str_detect, paste0(Result_String,"|DQ|DNS"))] %>% # must Results_String because all results do
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "\\d{3}\\.\\d{2}")))] %>% # closes loophole in Result_String where a number like 100.00 could get through even though it's not a valid result
          .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}")] %>% # must have at least two letters in a row
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>% # remove lines contained in avoid
          stringr::str_remove_all("\n") %>%
          stringr::str_remove_all("\\d{0,2}\\:?\\d{1,2}\\.\\d{3}") %>%
          # trimws() %>%
          stringr::str_replace_all(stats::setNames(replacement, typo)) %>%
          # remove 'A', 'B' etc. relay designators
          stringr::str_replace_all(" \\'[A-Z]\\' ", "  ") %>% # tf specific  - removes relay A, B etc. designators
          stringr::str_replace_all("  [A-Z]  ", "  ") %>%
          stringr::str_replace_all("\\'\\'", "  ") %>%
          stringr::str_remove_all("(?<=\\.\\d{2})[Q|q](?=\\s)") %>% # tf specific - removes "q" or "Q" sometimes used to designate a qualifying result
          stringr::str_remove_all("(?<=\\s)[J|j](?=\\d)") %>% # tf specific - removes "j" or "J" sometimes used to designate a judged result
          stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
          stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
          stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
          stringr::str_replace_all("\\*", "_") %>%
          stringr::str_replace_all(" \\+", "  \\+") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all(" \\-", "  \\-") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all("#", "  ") %>%  # tf specific, leading pound sign to spaces
          stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # tf specific - split place and name
          stringr::str_replace_all("(?<=\\dm) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split distance and windspeed
          stringr::str_replace_all("(?<=\\d) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split time and windspeed
          stringr::str_replace_all("(?<=\\d{3}) (?=[:alpha:])", "  ") %>% # tf specific - split bib number and name
          stringr::str_replace_all("(?<=\\d{2}) (?=[:alpha:])", "  ") %>% # tf specific - split age and team
          stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # tf specific - split name and age
          stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # tf specific - split name and age
          stringr::str_replace_all("(?<=\\d\\.\\d) (?=\\d{1,2}\\s)", "  ") %>% # tf specific - split off wind and heat number
          stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>% # tf specific - split off row_numb
          trimws()
      )

      #### splits data into variables by splitting at multiple (>= 2) spaces ####
      data_1 <-
        unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      unique(map(data_1, length))

      #### breaks data into subsets based on how many variables it has ####
      data_length_4 <- data_1[purrr::map(data_1, length) == 4]
      data_length_5 <- data_1[purrr::map(data_1, length) == 5]
      data_length_6 <- data_1[purrr::map(data_1, length) == 6]
      data_length_7 <- data_1[purrr::map(data_1, length) == 7]
      data_length_8 <- data_1[purrr::map(data_1, length) == 8]
      data_length_9 <- data_1[purrr::map(data_1, length) == 9]
      data_length_10 <- data_1[purrr::map(data_1, length) == 10]

      # treatment of DQs new 8/19
      suppressWarnings(DQ <-
                         data_1[stringr::str_detect(data_1, Time_Score_String, negate = TRUE) == TRUE])
      DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]


      #### ten variables ####
      if (length(data_length_10) > 0) {
        suppressWarnings(
          df_10 <- data_length_10 %>%
            list_transform() %>%
            mutate(Place = V1) %>%
            mutate(Bib_Number = case_when(str_detect(V2, "^\\d{1,3}$") ~ V2,
                                          TRUE ~ "NA")) %>%
            mutate(Name = case_when(str_detect(V2, Name_String) ~ V2,
                                    str_detect(V3, Name_String) ~ V3,
                                    TRUE ~ "NA")) %>%
            mutate(Age = case_when(str_detect(V3, Age_String) ~ V3,
                                   str_detect(V4, Age_String) ~ V4,
                                   TRUE ~ "NA")) %>%
            mutate(Team = case_when(str_detect(V3, Age_String) & str_detect(V4, "[:alpha:]{2,}") ~ V4,
                                    str_detect(V4, Age_String) & str_detect(V5, "[:alpha:]{2,}") ~ V5,
                                    TRUE ~ "NA")) %>%
            mutate(Prelims_Result = case_when(str_detect(V5, Result_Specials_String) & str_detect(V6, Result_Specials_String) ~ V5,
                                              str_detect(V6, Result_Specials_String) & str_detect(V7, Result_Specials_String) ~ V6,
                                              TRUE ~ "NA")) %>%
            mutate(Finals_Result = case_when(str_detect(V5, Result_Specials_String) == TRUE & str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                                             str_detect(V6, Result_Specials_String) & str_detect(V7, Result_Specials_String) ~ V7,
                                             TRUE ~ "NA")) %>%
            mutate(Wind_Speed = case_when(str_detect(V8, Wind_String) ~ V8,
                                          TRUE ~ "NA")) %>%
            mutate(Points = case_when(str_detect(V8, Wind_Speed) == FALSE & str_detect(V8, "^\\d\\.?\\d?$") == TRUE & str_detect(V9, "\\d{1,2}") == FALSE ~ V8,
                                      str_detect(V8, Wind_Speed) == TRUE & str_detect(V9, "^\\d\\.?\\d?$") == TRUE ~ V9,
                                      TRUE ~ "NA")) %>%
            mutate(Notes = case_when(str_detect(V9, Points) == FALSE ~ V9,
                                     TRUE ~ "NA")) %>%
            select(Place, Bib_Number, Name, Age, Team, Prelims_Result, Finals_Result, Wind_Speed, Points, Notes, 'Row_Numb' = V10) %>%
            na_if("^NA$")
        )

      } else {
        df_10 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }

      #### nine variables ####
      if (length(data_length_9) > 0) {
        suppressWarnings(
          df_9 <- data_length_9 %>%
            list_transform() %>%
            mutate(Place = V1) %>%
            mutate(Bib_Number = case_when(str_detect(V2, "^\\d{1,3}$") ~ V2,
                                          TRUE ~ "NA")) %>%
            mutate(Name = case_when(str_detect(V2, Name_String) ~ V2,
                                    str_detect(V3, Name_String) ~ V3,
                                    TRUE ~ "NA")) %>%
            mutate(Age = case_when(str_detect(V3, Age_String) ~ V3,
                                   str_detect(V4, Age_String) ~ V4,
                                   TRUE ~ "NA")) %>%
            mutate(Team = case_when(str_detect(V3, Age_String) & str_detect(V4, "[:alpha:]{2,}") ~ V4,
                                    str_detect(V4, Age_String) & str_detect(V5, "[:alpha:]{2,}") ~ V5,
                                    TRUE ~ "NA")) %>%
            mutate(Prelims_Result = case_when(str_detect(V6, Result_Specials_String) & str_detect(V7, Result_Specials_String) ~ V6,
                                              TRUE ~ "NA")) %>%
            mutate(Finals_Result = case_when(str_detect(V6, Result_Specials_String) & str_detect(V7, Result_Specials_String) ~ V7,
                                             str_detect(V6, Result_Specials_String) == TRUE & str_detect(V5, Result_Specials_String) == FALSE & str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                                             TRUE ~ "NA")) %>%
            mutate(Wind_Speed = case_when(str_detect(V7, Wind_String) ~ V7,
                                          str_detect(V7, Wind_String) == FALSE & str_detect(V8, Wind_String) == TRUE ~ V8,
                                          TRUE ~ "NA")) %>%
            mutate(Heat = case_when(str_detect(V7, Wind_String) == FALSE & str_detect(V7, "^\\d{1,2}$") == TRUE & str_detect(V8, "^\\d\\d?\\.?\\d?$") == TRUE ~ V7,
                                      TRUE ~ "NA")) %>%
            mutate(Points = case_when(str_detect(V7, Heat) == TRUE & str_detect(V8, "^\\d\\d?\\.?\\d?$") ~ V8,
                                    TRUE ~ "NA")) %>%
            mutate(Notes = case_when(str_detect(V8, Points) == FALSE & str_detect(V8, "^\\d\\.?\\d?$") == FALSE ~ V8,
                                     TRUE ~ "NA")) %>%
            select(Place, Bib_Number, Name, Age, Team, Prelims_Result, Finals_Result, Wind_Speed, Heat, Points, Notes, 'Row_Numb' = V9) %>%
            na_if("^NA$")
        )

      } else {
        df_9 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### eight variables ####
      if (length(data_length_8) > 0) {
        suppressWarnings(
          df_8<- data_length_8 %>%
            list_transform() %>%
            mutate(Place = V1) %>%
            mutate(Bib_Number = case_when(str_detect(V2, "^\\d{1,3}$") ~ V2,
                                          TRUE ~ "NA")) %>%
            mutate(Name = case_when(str_detect(V2, Name_String) ~ V2,
                                    str_detect(V3, Name_String) ~ V3,
                                    TRUE ~ "NA")) %>%
            mutate(Age = case_when(str_detect(V3, Age_String) ~ V3,
                                   str_detect(V4, Age_String) ~ V4,
                                   TRUE ~ "NA")) %>%
            mutate(Team = case_when(str_detect(V3, Age_String) & str_detect(V4, "[:alpha:]{2,}") ~ V4,
                                    str_detect(V4, Age_String) & str_detect(V5, "[:alpha:]{2,}") ~ V5,
                                    TRUE ~ "NA")) %>%
            mutate(Finals_Result = case_when(str_detect(V5, Result_Specials_String) & str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                                             str_detect(V6, Result_Specials_String) == TRUE & str_detect(V5, Result_Specials_String) == FALSE & str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                                             TRUE ~ "NA")) %>%
            mutate(Wind_Speed = case_when(str_detect(V7, Wind_String) ~ V7,
                                          TRUE ~ "NA")) %>%
            mutate(Notes = case_when(str_detect(V7, Wind_String) == FALSE & str_detect(V7, "^\\d\\.?\\d?$") == FALSE ~ V7,
                                     TRUE ~ "NA")) %>%
            select(Place, Bib_Number, Name, Age, Team, Finals_Result, Wind_Speed, Notes, 'Row_Numb' = V8) %>%
            na_if("^NA$")
        )

      } else {
        df_8 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### seven variables ####
      if (length(data_length_7) > 0) {
        suppressWarnings(
          df_7 <- data_length_7 %>%
            list_transform() %>%
            mutate(Place = V1) %>%
            mutate(Bib_Number = case_when(str_detect(V2, "^\\d{1,3}$") ~ V2,
                                          TRUE ~ "NA")) %>%
            mutate(Name = case_when(str_detect(V2, Name_String) ~ V2,
                                    str_detect(V3, Name_String) ~ V3,
                                    TRUE ~ "NA")) %>%
            mutate(Age = case_when(str_detect(V3, Age_String) ~ V3,
                                   str_detect(V4, Age_String) ~ V4,
                                   TRUE ~ "NA")) %>%
            mutate(Team = case_when(str_detect(V3, Age_String) & str_detect(V4, "[:alpha:]{2,}") ~ V4,
                                    str_detect(V4, Age_String) & str_detect(V5, "[:alpha:]{2,}") ~ V5,
                                    TRUE ~ "NA")) %>%
            mutate(Finals_Result = case_when(str_detect(V5, Result_Specials_String) & str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                                             str_detect(V6, Result_Specials_String) == TRUE & str_detect(V5, Result_Specials_String) == FALSE & str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                                             TRUE ~ "NA")) %>%
            mutate(Wind_Speed = case_when(str_detect(V6, Wind_String) ~ V6,
                                          TRUE ~ "NA")) %>%
            select(Place, Bib_Number, Name, Age, Team, Finals_Result, Wind_Speed, 'Row_Numb' = V7) %>%
            na_if("^NA$")
        )

      } else {
        df_7 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### six variables ####
      if (length(data_length_6) > 0) {
        suppressWarnings(
          df_6 <- data_length_6 %>%
            list_transform() %>%
            mutate(Place = V1) %>%
            mutate(Name = case_when(str_detect(V2, Name_String) ~ V2,
                                    str_detect(V3, Name_String) ~ V3,
                                    TRUE ~ "NA")) %>%
            mutate(Age = case_when(str_detect(V3, Age_String) ~ V3,
                                   str_detect(V4, Age_String) ~ V4,
                                   TRUE ~ "NA")) %>%
            mutate(Team = case_when(str_detect(V3, Age_String) & str_detect(V4, "[:alpha:]{2,}") ~ V4,
                                    str_detect(V4, Age_String) & str_detect(V5, "[:alpha:]{2,}") ~ V5,
                                    TRUE ~ "NA")) %>%
            mutate(Team = case_when(Team == "NA"  ~ Name,
                                    TRUE ~ Team),
                   Name = case_when(Team == Name~ "NA",
                                    TRUE ~ Name)) %>%
            mutate(Age = case_when(Name == "NA" ~ "NA",
                                   TRUE ~ Age)) %>%
            mutate(Prelims_Result = case_when(str_detect(V3, Result_Specials_String) == TRUE & str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                                              TRUE ~ "NA")) %>%
            mutate(Finals_Result = case_when(str_detect(V3, Result_Specials_String) == TRUE & str_detect(V4, Result_Specials_String) == TRUE ~ V4,
                                             str_detect(V4, Result_Specials_String) == FALSE & str_detect(V5, Result_Specials_String) == TRUE ~ V5)) %>%
            select(Place, Name, Age, Team, Prelims_Result, Finals_Result, 'Row_Numb' = V6) %>%
            na_if("^NA$")
        )

      } else {
        df_6 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### five variables ####
      if (length(data_length_5) > 0) {
        suppressWarnings(
          df_5 <- data_length_5 %>%
            list_transform() %>%
            select("Place" = V1, "Team" = V2, "Finals_Result" = V3, "Row_Numb" = V5)
        )

      } else {
        df_5<- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### four variables ####
      if (length(data_length_4) > 0) {
        suppressWarnings(
          df_4 <- data_length_4 %>%
            list_transform() %>%
            select("Place" = V1, "Team" = V2, "Finals_Result" = V3, "Row_Numb" = V4)
        )

      } else {
        df_4 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      Min_Row_Numb <- min(events$Event_Row_Min)
      suppressWarnings(
        data <- dplyr::bind_rows(df_10, df_9, df_8, df_7, df_6, df_5, df_4) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb) %>%
          dplyr::mutate(Exhibition = 0) %>%
          dplyr::mutate(DQ = 0) %>%
          ### moved up from below for DQ work 8/20
          dplyr::mutate(DQ = dplyr::case_when(Place == 10000 & Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                              TRUE ~ DQ)) %>%
          na_if(10000) %>%
          dplyr::mutate(dplyr::across(c(Name, Team), ~ stringr::str_replace_all(., "10000", "--"))) %>% # remove any "10000"s added in erroniuously
          ####
          na_if("DNS") %>%
          na_if("DNF") %>%
          na_if("DQ") %>%
          dplyr::mutate(
            Place = as.numeric(Place),
            Place = dplyr::case_when(
              is.na(dplyr::lag(Place)) == TRUE ~ Place,
              dplyr::lag(Place) == Place ~ Place + 0.1,
              dplyr::lag(Place) != Place ~ Place
            ),
            Place = as.character(Place),
            Row_Numb = as.numeric(Row_Numb)
          ) %>%
          dplyr::filter(Row_Numb >= Min_Row_Numb)
      )

      if("Points" %in% names(data) == FALSE)
      {data$Points <- NA}

      #### add in events based on row number ranges ####
      data  <-
        transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)])

  }
