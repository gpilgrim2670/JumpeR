#' Parses Hytek format track and field results inside \code{tf_parse}
#'
#' Outputs a data frame of track and field results
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom dplyr all_of
#' @importFrom dplyr starts_with
#' @importFrom dplyr matches
#' @importFrom dplyr distinct
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr str_length
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_subset
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom stats setNames
#' @importFrom SwimmeR `%!in%`
#'
#' @param hytek_file data with row numbers added
#' @param hytek_relay_athletes should \code{tf_parse} try to include the names
#'   of relay athletes for relay events?  Names will be listed in new columns
#'   "Relay-Athlete_1", "Relay_Athlete_2" etc.  Defaults to \code{FALSE}.
#' @param hytek_rounds should \code{tf_parse} try to include rounds for
#'   jumping/throwing events?  Please note this will add a significant number of
#'   columns to the resulting data frame.  Defaults to \code{FALSE}.
#' @param hytek_round_attempts should \code{tf_parse} try to include rounds
#'   results (i.e. "PASS", "X", "O") for high jump and pole value events?
#'   Please note this will add a significant number of columns to the resulting
#'   data frame. Defaults to \code{FALSE}
#' @param hytek_split_attempts should \code{tf_parse} split attempts from each
#'   round into separate columns?  For example "XXO" would result in three
#'   columns, one for "X', another for the second "X" and third for "O".  There
#'   will be a lot of columns.  Defaults to \code{FALSE}
#' @param hytek_splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{hytek_parse} attempt to include splits.
#' @param hytek_split_length either the distance at which splits are collected
#'   (must be constant distance) or the default, \code{1}, the length of track
#'   at which splits are recorded.  Not all results are internally consistent on
#'   this issue.  If in doubt use the default \code{1}
#'
#' @return a data frame of track and field results
#'
#' @seealso \code{hytek_parse} is runs inside
#'   \code{\link{tf_parse}}



hytek_parse <-
  function(hytek_file = file,
           hytek_relay_athletes = relay_athletes,
           hytek_rounds = rounds,
           hytek_round_attempts = round_attempts,
           hytek_split_attempts = split_attempts,
           hytek_splits = splits,
           hytek_split_length = split_length) {

    #### testing setup ####
    # hytek_file <-
    #   "http://results.deltatiming.com/ncaa/tf/2019-joe-walker-invitational/190412F009" %>%
    #   read_results() %>%
    #   add_row_numbers()

    # hytek_file <-
    #   "http://tfresultsdata.deltatiming.com/2018-hurricane-invitational/180316F028.htm" %>%
    #   read_results() %>%
    #   add_row_numbers()
    # hytek_file <- "http://tfresultsdata.deltatiming.com/2019-ysu-icebreaker/191206F031.htm" %>%
    #   read_results() %>%
    #   add_row_numbers()


        #### Pulls out event labels from text ####

    raw_results <- hytek_file

        events <- event_parse(raw_results) %>%
          dplyr::mutate(Event = stringr::str_remove(Event, " Women$| Men$"))


        #### set up strings ####
        Name_String <-
          "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
        Result_String <- "\\d{0,2}\\:?\\-?\\d{1,2}\\.\\d{2}m?"
        Result_Specials_String <-
          paste0(Result_String, "$|^NT$|^NP$|^DQ$|^DNS$|^DNF$|^FOUL$|^NH$|^SCR$|^FS$|^ND$")
        Wind_String <-
          "\\+\\d\\.\\d|\\-\\d\\.\\d|^NWS$|^NWI$|^\\d\\.\\d$"
        Age_String <- "^SR$|^JR$|^SO$|^FR$|^M?W?[:digit:]{1,3}$"
        Tiebreaker_String <- "\\d\\.\\d{3}$"

        suppressWarnings(
          data_1 <- raw_results %>%
            .[purrr::map(., length) > 0] %>%
            .[stringr::str_length(.) > 50] %>%
            .[stringr::str_count(., "\\d\\)") < 2] %>%  # remove inline splits and team scores as 1) Alfred 2) Ithaca etc.
            .[stringr::str_detect(., paste0(Result_String, "|DQ|DNS|DNF|FOUL|NH|SCR|FS|ND"))] %>% # must Results_String because all results do
            .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "\\d{3}\\.\\d{2}")))] %>% # closes loophole in Result_String where a number like 100.00 could get through even though it's not a valid result
            .[purrr::map_lgl(., ~ !any(
              stringr::str_detect(., "^[0-9\\(\\)\\.FOULPASSm\\s\\-\\+]+$")
            ))] %>% # closes loophole where throwing splits lines get in because they contain FOUL or PASS plus valid results
            .[purrr::map_lgl(., ~ !any(
              stringr::str_detect(., "Event .*\\d")
            ))] %>% # removes event titles that also include distances, like "Event 1 Short Hurdles 0.762m"
            .[stringr::str_detect(., "[:alpha:]{2,}")] %>% # must have at least two letters in a row
            # .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>% # remove lines contained in avoid
            stringr::str_remove_all("\n\\s*") %>%
            # stringr::str_remove_all("\\d{0,2}\\:?\\d{1,2}\\.\\d{3}") %>% # ties
            # trimws() %>%
            # stringr::str_replace_all(stats::setNames(replacement, typo)) %>%
            # remove 'A', 'B' etc. relay designators
            stringr::str_replace_all(" \\'[A-Z]\\' ", "  ") %>% # tf specific  - removes relay A, B etc. designators
            stringr::str_replace_all("  [A-Z]  ", "  ") %>%
            stringr::str_replace_all("\\'\\'", "  ") %>%
            # stringr::str_remove_all("(?<=\\.\\d{2})[Q|R|M|E|\\$|q](?=\\s)") %>% # tf specific - removes "q" or "Q" sometimes used to designate a qualifying result, also 'R', "M", "$", "E"
            stringr::str_remove_all("(?<=\\.\\d{2})[A-Z|\\$|q](?=\\s)") %>% # tf specific - removes "q" or "Q" sometimes used to designate a qualifying result, also 'R', "M", "$", "E"
            stringr::str_replace_all("(?<=\\s{3})[A-Z|\\$|q] (?=\\d)", "   ") %>% # tf specific - removes upper case letters sometimes used before times to designate records etc.
            stringr::str_remove_all("(?<=\\s)[J|j](?=\\d)") %>% # tf specific - removes "j" or "J" sometimes used to designate a judged result
            stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
            # stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
            # stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
            stringr::str_replace_all("\\*", "_") %>%
            stringr::str_replace_all(" \\+", "  \\+") %>%  # tf speciifc, for windspeed
            stringr::str_replace_all(" \\-", "  \\-") %>%  # tf speciifc, for windspeed
            stringr::str_replace_all("#", "  ") %>%  # tf specific, leading pound sign to spaces
            stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # tf specific - split place and name
            stringr::str_replace_all("(?<=\\dm) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split distance and windspeed
            stringr::str_replace_all("(?<=\\d) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split time and windspeed
            stringr::str_replace_all("(?<=\\d{3}) (?=[:alpha:])", "  ") %>% # tf specific - split bib number and name
            stringr::str_replace_all("(?<=\\d{2}) (?=[:alpha:])", "  ") %>% # tf specific - split age and team
            stringr::str_replace_all("(?<=\\.\\d{3})s ", "  ") %>% # tf specific - split tiebreaker and s designator
            stringr::str_replace_all(" FR ", "  FR  ") %>% # tf specific - split age and team
            stringr::str_replace_all(" SO ", "  SO  ") %>% # tf specific - split age and team
            stringr::str_replace_all(" JR ", "  JR  ") %>% # tf specific - split age and team
            stringr::str_replace_all(" SR ", "  SR  ") %>% # tf specific - split age and team
            stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # tf specific - split name and age
            stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # tf specific - split name and age
            stringr::str_replace_all(" (M\\d{1,3}) ", "  \\1  ") %>% # tf specific - gendered ages M
            stringr::str_replace_all(" (W\\d{1,3}) ", "  \\1  ") %>% # tf specific - gendered ages W
            stringr::str_replace_all("(?<=\\d\\.\\d) (?=\\d{1,2}\\s)", "  ") %>% # tf specific - split off wind and heat number
            stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>% # tf specific - split off row_numb
            stringr::str_replace_all("(?<=\\dm)[:upper:](?=\\s)", "  ") %>% # tf specific - sometimes an M for Meet record is added to a distance 1.23m as 1.23mM
            stringr::str_replace_all("(?<=\\dm)[:graph:](?=\\s)", "  ") %>% # tf specific - sometimes an M for Meet record is added to a distance 1.23m as 1.23mM
            stringr::str_replace_all(" \\., ", "  Period, ") %>% # for people with no first/last name, like Indian runners in some Singapore results
            stringr::str_replace_all("([:alpha])(\\.[:alpha:])", "\\1 \\2") %>%
            trimws()
        )

        #### if data_1 is empty ####
        if(!length(data_1) > 0){
          message("No results found in file")

        } else {

        #### splits data into variables by splitting at multiple (>= 2) spaces ####
        data_1 <-
          unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
                 recursive = FALSE)

        # unique(map(data_1, length))

        #### breaks data into subsets based on how many variables it has ####
        data_length_4 <- data_1[purrr::map(data_1, length) == 4]
        data_length_5 <- data_1[purrr::map(data_1, length) == 5]
        data_length_6 <- data_1[purrr::map(data_1, length) == 6]
        data_length_7 <- data_1[purrr::map(data_1, length) == 7]
        data_length_8 <- data_1[purrr::map(data_1, length) == 8]
        data_length_9 <- data_1[purrr::map(data_1, length) == 9]
        data_length_10 <- data_1[purrr::map(data_1, length) == 10]

        # treatment of DQs
        # suppressWarnings(DQ <-
        #                    data_1[stringr::str_detect(data_1, Time_Score_String, negate = TRUE) == TRUE])
        # DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
        # DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]


        #### ten variables ####
        if (length(data_length_10) > 0) {
          suppressWarnings(
            df_10 <- data_length_10 %>%
              list_transform() %>%
              dplyr::mutate(Place = V1) %>%
              dplyr::mutate(
                Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") ~ V2,
                                              TRUE ~ "NA")
              ) %>%
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) ~ V2,
                  stringr::str_detect(V3, Name_String) ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Age = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) ~ V3,
                  stringr::str_detect(V4, Age_String) ~ V4,
                  stringr::str_detect(V5, Age_String) ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) &
                    stringr::str_detect(V4, "[:alpha:]{2,}") ~ V4,
                  stringr::str_detect(V4, Age_String) &
                    stringr::str_detect(V5, "[:alpha:]{2,}") ~ V5,
                  stringr::str_detect(V5, Age_String) &
                    stringr::str_detect(V6, "[:alpha:]{2,}") ~ V6,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Prelims_Result = dplyr::case_when(
                  stringr::str_detect(V5, Result_Specials_String) &
                    stringr::str_detect(V6, Result_Specials_String) ~ V5,
                  stringr::str_detect(V6, Result_Specials_String) &
                    stringr::str_detect(V7, Result_Specials_String) ~ V6,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == FALSE &
                    stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                  stringr::str_detect(V6, Result_Specials_String) &
                    stringr::str_detect(V7, Result_Specials_String) ~ V7,
                  stringr::str_detect(V7, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, Result_Specials_String) == FALSE &
                    stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Wind_Speed = dplyr::case_when(stringr::str_detect(V8, Wind_String) ~ V8,
                                              TRUE ~ "NA")
              ) %>%
              dplyr::mutate(
                Points = dplyr::case_when(
                  stringr::str_detect(V8, Wind_String) == FALSE &
                    stringr::str_detect(V8, "^\\d\\.?\\d?$") == TRUE &
                    stringr::str_detect(V9, "\\d{1,2}") == FALSE ~ V8,
                  stringr::str_detect(V8, Wind_String) == TRUE &
                    stringr::str_detect(V9, "^\\d\\.?\\d?$") == TRUE ~ V9,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Tiebreaker = dplyr::case_when(stringr::str_detect(V9, Tiebreaker_String) == TRUE ~ V9,
                                              TRUE ~ "NA")
              ) %>%
              dplyr::select(
                Place,
                Bib_Number,
                Name,
                Age,
                Team,
                Prelims_Result,
                Finals_Result,
                Wind_Speed,
                Points,
                Tiebreaker,
                'Row_Numb' = V10
              )
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
              dplyr::mutate(Place = V1) %>%
              dplyr::mutate(
                Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") ~ V2,
                                              TRUE ~ "NA")
              ) %>%
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) ~ V2,
                  stringr::str_detect(V3, Name_String) ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Age = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) ~ V3,
                  stringr::str_detect(V4, Age_String) ~ V4,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) &
                    stringr::str_detect(V4, "[:alpha:]{2,}") ~ V4,
                  stringr::str_detect(V4, Age_String) &
                    stringr::str_detect(V5, "[:alpha:]{2,}") ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Prelims_Result = dplyr::case_when(
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V6,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V6, Result_Specials_String) &
                    stringr::str_detect(V7, Result_Specials_String) ~ V7,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == FALSE &
                    stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, Result_Specials_String) == FALSE &
                    stringr::str_detect(V4, Result_Specials_String) == FALSE ~ V5,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, "m") == TRUE &
                    stringr::str_detect(V6, "-") == TRUE ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Wind_Speed = dplyr::case_when(
                  stringr::str_detect(V7, Wind_String) ~ V7,
                  stringr::str_detect(V7, Wind_String) == FALSE &
                    stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Heat = dplyr::case_when(
                  stringr::str_detect(V7, Wind_String) == FALSE &
                    stringr::str_detect(V7, "^\\d{1,2}$") == TRUE &
                    stringr::str_detect(V8, "^\\d\\d?\\.?\\d?$") == TRUE ~ V7,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Points = dplyr::case_when(
                  stringr::str_detect(V7, Heat) == TRUE &
                    stringr::str_detect(V8, "^\\d\\d?\\.?\\d?$") ~ V8,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Tiebreaker = dplyr::case_when(
                  stringr::str_detect(V8, Tiebreaker_String) == TRUE ~ V8,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::select(
                Place,
                Bib_Number,
                Name,
                Age,
                Team,
                Prelims_Result,
                Finals_Result,
                Wind_Speed,
                Heat,
                Points,
                Tiebreaker,
                'Row_Numb' = V9
              )
          )

        } else {
          df_9 <- data.frame(Row_Numb = character(),
                             stringsAsFactors = FALSE)
        }

        #### eight variables ####
        if (length(data_length_8) > 0) {
          suppressWarnings(
            df_8 <- data_length_8 %>%
              list_transform() %>%
              dplyr::mutate(Place = V1) %>%
              dplyr::mutate(Bib_Number = dplyr::case_when(
                stringr::str_detect(V2, "^\\d{1,6}$") ~ V2,
                TRUE ~ "NA"
              )) %>%
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) ~ V2,
                  stringr::str_detect(V3, Name_String) ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Age = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) ~ V3,
                  stringr::str_detect(V4, Age_String) ~ V4,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) &
                    stringr::str_detect(V4, "[:alpha:]{2,}") ~ V4,
                  stringr::str_detect(V4, Age_String) &
                    stringr::str_detect(V5, "[:alpha:]{2,}") ~ V5,
                  stringr::str_detect(V2, Name_String) == TRUE &
                    stringr::str_detect(V3, Age_String) == FALSE &
                    stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V4, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == FALSE ~ V4,
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == FALSE &
                    stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, "m") == TRUE &
                    stringr::str_detect(V6, "-") == TRUE ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Wind_Speed = dplyr::case_when(
                  stringr::str_detect(V5, Wind_String) == TRUE &
                    stringr::str_detect(V6, Wind_String) == FALSE  ~ V5,
                  stringr::str_detect(V6, Wind_String) == TRUE &
                    stringr::str_detect(V7, Wind_String) == FALSE  ~ V6,
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Tiebreaker = dplyr::case_when(
                  stringr::str_detect(V7, Tiebreaker_String) == TRUE ~ V7,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::select(
                Place,
                Bib_Number,
                Name,
                Age,
                Team,
                Finals_Result,
                Wind_Speed,
                Tiebreaker,
                'Row_Numb' = V8
              )
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
              dplyr::mutate(Place = V1) %>%
              dplyr::mutate(
                Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") ~ V2,
                                              TRUE ~ "NA")
              ) %>%
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) ~ V2,
                  stringr::str_detect(V3, Name_String) ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Age = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) ~ V3,
                  stringr::str_detect(V4, Age_String) ~ V4,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) == FALSE &
                    # stringr::str_detect(V4, Age_String) == FALSE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE &
                    stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3,
                  stringr::str_detect(V3, Age_String) == TRUE &
                    stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                  stringr::str_detect(V3, Name_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                  stringr::str_detect(V4, Age_String) == TRUE &
                    stringr::str_detect(V5, "[:alpha:]{2,}") == TRUE ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V4,
                  stringr::str_detect(V5, Result_Specials_String) == TRUE & # to deal with results that have both metric and imperial result - prefer metric
                    stringr::str_detect(V5, "m") == TRUE  &
                    stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, "m") == FALSE ~ V5,
                  stringr::str_detect(V5, Result_Specials_String) &
                    stringr::str_detect(V5, "m") == FALSE  &
                    stringr::str_detect(V6, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, "m") == TRUE ~ V6,
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                  stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V6,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Wind_Speed = dplyr::case_when(
                  stringr::str_detect(V5, Wind_String) == TRUE &
                    stringr::str_detect(V6, Wind_String) == FALSE ~ V5,
                  stringr::str_detect(V6, Wind_String) ~ V6,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::select(
                Place,
                Bib_Number,
                Name,
                Age,
                Team,
                Finals_Result,
                Wind_Speed,
                'Row_Numb' = V7
              )
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
              dplyr::mutate(Place = V1) %>%
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) ~ V2,
                  stringr::str_detect(V3, Name_String) ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(Bib_Number = case_when(str_detect(V2, "^\\d{1,6}$") ~ V2,
                                                   TRUE ~ "NA")) %>%
              dplyr::mutate(
                Age = dplyr::case_when(
                  stringr::str_detect(V3, Age_String) ~ V3,
                  stringr::str_detect(V4, Age_String) ~ V4,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V3, Result_Specials_String) == TRUE ~ V2,
                  stringr::str_detect(V2, Name_String) == TRUE &
                    stringr::str_detect(V3, Age_String) == FALSE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                  stringr::str_detect(V3, Age_String) &
                    stringr::str_detect(V4, "[:alpha:]{2,}") ~ V4,
                  stringr::str_detect(V3, Name_String) == TRUE &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, "[:alpha:]{2,}") ~ V4,
                  stringr::str_detect(V4, Age_String) &
                    stringr::str_detect(V3, Result_Specials_String) == FALSE &
                    stringr::str_detect(V5, "[:alpha:]{2,}") ~ V5,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(Team == "NA"  ~ Name,
                                        TRUE ~ Team),
                Name = dplyr::case_when(Team == Name ~ "NA",
                                        TRUE ~ Name)
              ) %>%
              dplyr::mutate(Age = dplyr::case_when(Name == "NA" ~ "NA",
                                                   TRUE ~ Age)) %>%
              dplyr::mutate(
                Prelims_Result = dplyr::case_when(
                  stringr::str_detect(V3, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V4, Result_Specials_String) &
                    stringr::str_detect(V4, "m") == TRUE  &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, "m") == FALSE ~ V4,
                  stringr::str_detect(V4, Result_Specials_String) &
                    stringr::str_detect(V4, "m") == FALSE  &
                    stringr::str_detect(V5, Result_Specials_String) == TRUE &
                    stringr::str_detect(V5, "m") == TRUE ~ V5,
                  stringr::str_detect(V3, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == FALSE ~ V3,
                  stringr::str_detect(V3, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V4,
                  stringr::str_detect(V3, Result_Specials_String) == FALSE &
                    stringr::str_detect(V5, Result_Specials_String) == FALSE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V4,
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V5
                )
              ) %>%
              dplyr::select(
                Place,
                Bib_Number,
                Name,
                Age,
                Team,
                Prelims_Result,
                Finals_Result,
                'Row_Numb' = V6
              )
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
              dplyr::mutate(
                Name = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V2,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Team = dplyr::case_when(
                  stringr::str_detect(V2, Name_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                  TRUE ~ V2
                )
              ) %>%
              dplyr::mutate(
                Prelims_Result = dplyr::case_when(
                  stringr::str_detect(V3, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::mutate(
                Finals_Result = dplyr::case_when(
                  stringr::str_detect(V3, Result_Specials_String) == TRUE &
                    stringr::str_detect(V4, Result_Specials_String) == FALSE ~ V3,
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V4,
                  TRUE ~ "NA"
                )
              ) %>%
              dplyr::select(
                "Place" = V1,
                Name,
                Team,
                Prelims_Result,
                Finals_Result,
                "Row_Numb" = V5
              )
          )

        } else {
          df_5 <- data.frame(Row_Numb = character(),
                             stringsAsFactors = FALSE)
        }

        #### four variables ####
        if (length(data_length_4) > 0) {
          suppressWarnings(
            df_4 <- data_length_4 %>%
              list_transform() %>%
              dplyr::select(
                "Place" = V1,
                "Team" = V2,
                "Finals_Result" = V3,
                "Row_Numb" = V4
              )
          )

        } else {
          df_4 <- data.frame(Row_Numb = character(),
                             stringsAsFactors = FALSE)
        }

        Min_Row_Numb <- min(events$Event_Row_Min)
        suppressWarnings(
          data <-
            dplyr::bind_rows(df_10, df_9, df_8, df_7, df_6, df_5, df_4) %>%
            dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
            dplyr::arrange(Row_Numb) %>%
            dplyr::mutate(Exhibition = 0) %>%
            dplyr::mutate(DQ = 0) %>%
          { # Place column might or might not exist
            if("Place" %!in% names(.)) dplyr::mutate(., Place = "NA") else . # sometimes there's no place column
            } %>%
            dplyr::filter(Place != "ND") %>%
            ### moved up from below for DQ work 8/20
            dplyr::mutate(
              DQ = dplyr::case_when(
                Place == 10000 &
                  Exhibition == 0  & # added exhibition condition 8/27
                  stringr::str_detect(Finals_Result, "DNS") == FALSE ~ 1, # DNS is not charged as an event, is not a DQ
                TRUE ~ DQ
              )
            ) %>%
            dplyr::na_if(10000) %>%
            { # Names column might or might not exist
              if("Name" %!in% names(.)) dplyr::mutate(., Name = "NA") else . # relay entries don't have a team column
            } %>%
            dplyr::mutate(dplyr::across(
              c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
            )) %>% # remove any "10000"s added in erroneously
            ####
            # dplyr::na_if("DNS") %>%
            # dplyr::na_if("DNF") %>%
            # dplyr::na_if("DQ") %>%
            # dplyr::na_if("FOUL") %>%
            # dplyr::na_if("NH") %>%
            # dplyr::na_if("NWI") %>%
            # dplyr::na_if("SCR") %>%
            # dplyr::na_if("FS") %>%
            dplyr::mutate(
              # Place = as.numeric(Place), # from swim_parse for dealing with ties
              # Place = dplyr::case_when(
              #   is.na(dplyr::lag(Place)) == TRUE ~ Place,
              #   dplyr::lag(Place) == Place ~ Place + 0.1,
              #   dplyr::lag(Place) != Place ~ Place
              # ),
              # Place = as.character(Place),
              Row_Numb = as.numeric(Row_Numb)
            ) %>%
            dplyr::filter(Row_Numb >= Min_Row_Numb) %>%
            dplyr::filter(stringr::str_detect(Place, "\\.") %in% c(FALSE, NA)) %>%
            dplyr::na_if("NA")
        )

        #### Address Gendered Ages
        if("Age" %in% names(data)){
          data <- data %>%
            dplyr::mutate(Gender = stringr::str_extract(Age, "^M|^W")) %>%
            dplyr::mutate(Age = dplyr::case_when(
              is.na(Gender) == FALSE ~ stringr::str_remove(Age, Gender),
              TRUE ~ Age
            ))
        }

        #### Address Names with "." renamed to "Period"
        if("Name" %in% names(data)){
          data <- data %>%
            dplyr::mutate(Name = stringr::str_replace(Name, "Period", "\\."))
        }

        #### added in to work with arrange/distinct calls after adding in events ####
        if("Prelims_Result" %in% names(data) == FALSE){
          data$Prelims_Result <- NA
        }

        if("Wind_Speed" %in% names(data) == FALSE){
          data$Wind_Speed <- NA
        }

        #### add in events based on row number ranges ####
        data  <-
          transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
          dplyr::arrange(Name, Team, is.na(Wind_Speed), is.na(Prelims_Result)) %>% # new 1/1/21 to deal with results presented by heat and as final on same page
          dplyr::distinct(Name, Team, Event, Prelims_Result, Finals_Result, .keep_all = TRUE) %>%  # new 1/1/21 to deal with results presented by heat and as final on same page
          dplyr::arrange(Row_Numb)

        #### adding relay athletes in ####
        if (hytek_relay_athletes == TRUE) {
          relay_athletes_df <- collect_relay_athletes(raw_results)

          relay_athletes_df <-
            transform(relay_athletes_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
            dplyr::select(-Row_Numb)

          data <- data %>%
            dplyr::left_join(relay_athletes_df, c("Row_Numb" = "Row_Numb_Adjusted"))
        }

        #### adding splits back in ####
        if (hytek_splits == TRUE) {

          splits_df <- splits_parse(raw_results, split_len = hytek_split_length) %>%
            dplyr::distinct(dplyr::across(-Row_Numb), .keep_all = TRUE)

          #### matches row numbers in splits_df to available row numbers in data
          # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
          # and if those swimmers are listed on one line or two
          splits_df  <-
            transform(splits_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
            dplyr::select(-Row_Numb)

          data <- data %>%
            dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
            dplyr::select(!dplyr::starts_with("Split"), stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order

        }

        #### adding in rounds ####
        if(hytek_rounds == TRUE){
          rounds_df <- rounds_parse(raw_results)

          rounds_df <-
            transform(rounds_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
            dplyr::select(-Row_Numb)

          data <- dplyr::left_join(data, rounds_df, by = c("Row_Numb" = "Row_Numb_Adjusted"))
        }

        #### adding in rounds results ####
        if(hytek_round_attempts == TRUE){
          round_attempts_df <- round_attempts_parse(raw_results)

          round_attempts_df <-
            transform(round_attempts_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
            dplyr::select(-Row_Numb)

          data <- dplyr::left_join(data, round_attempts_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
            dplyr::mutate(dplyr::across(dplyr::contains("_Attempts"), ~ dplyr::case_when(stringr::str_detect(Event, "ong|riple") ~ "Unknown",
                                                                                       stringr::str_detect(Event, "ong|riple", negate = TRUE) ~ .))) %>%
            na_if("Unknown")
        }

        if(hytek_split_attempts == TRUE){
          suppressMessages(data <- attempts_split(data))
        }


        # removes unneeded Round_X columns (i.e. those that don't have an associated Round_Result)
        if (any(stringr::str_detect(names(data), "Round_\\d{1,}_Attempt")) == TRUE) {
          data <- remove_unneeded_rounds(data) %>%
            dplyr::na_if("")
        }

        # add in wind for rounds
        if(hytek_rounds == TRUE){
        round_wind_df <- wind_parse_hytek(raw_results)

        round_wind_df <-
          transform(round_wind_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
          dplyr::select(-Row_Numb)

        data <- dplyr::left_join(data, round_wind_df, by = c("Row_Numb" = "Row_Numb_Adjusted"))
        }

        #### ordering columns after adding rounds ####
        if (hytek_rounds == TRUE) {
          data <- data %>%
            dplyr::select(colnames(.)[stringr::str_detect(names(.), "^Round", negate = TRUE)], stringr::str_sort(colnames(.)[stringr::str_detect(names(.), "^Round")]))
        }

        #### remove empty columns (all values are NA) ####
        data <- Filter(function(x)
          !all(is.na(x)), data)


        #### remove unneeded columns ####
        data <- data %>%
          dplyr::select(which(SwimmeR::`%!in%`(
            names(.), c("Row_Numb", "Exhibition", "Points", "Heat")
          )))

        row.names(data) <- NULL

        return(data)
        }
      }

