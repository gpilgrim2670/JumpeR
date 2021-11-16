#' Reads track and field results into a list of strings in preparation for
#' parsing with \code{tf_parse}
#'
#' Outputs list of strings to be processed by \code{tf_parse}
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
#' @param flash_file a .pdf or .html file (could be a url) where containing
#'   track and field results.  Must be formatted in a "normal" fashion - see
#'   vignette
#' @param flash_rounds should tf_parse try to include rounds for
#'   jumping/throwing events?  Defaults to \code{FALSE}
#' @param flash_round_attempts should tf_parse try to include outcomes for
#'   rounds for vertical jumping events?  Defaults to \code{FALSE}
#' @param flash_split_attempts should round_attempts columns be split into
#'   individual attempts
#'
#' @return a data frame of track and field results
#'
#' @seealso \code{tf_parse} is meant to be preceded by
#'   \code{\link{read_results}}
#'
#' @export

flash_parse <-
  function(flash_file,
           flash_rounds = rounds,
           flash_round_attempts = round_attempts,
           flash_split_attempts = split_attempts) {


    #### testing setup ####
    # flash_file <- read_results("https://www.flashresults.com/2020_Meets/Indoor/02-21_VTChallenge/031-1.pdf") %>%
    #   add_row_numbers()
    # flash_file <- raw_results[11] %>%
    #   unlist() %>%
    #   add_row_numbers()
    # flash_file <- read_results("https://www.flashresults.com/2019_Meets/Outdoor/04-12_TamuInvite/014-1.pdf") %>%
    #   add_row_numbers()
    # flash_file <- read_results("https://www.flashresults.com/2019_Meets/Outdoor/06-30_PreClassic/002-1.pdf") %>%
    # #   add_row_numbers()
    # flash_file <- read_results("https://www.flashresults.com/2018_Meets/Outdoor/04-20_DukeInvite/014-1.pdf") %>%
    #   add_row_numbers()

    #### Begin actual function ####

    #### Pulls out event labels from text ####
    events <- event_parse(flash_file) %>%
      dplyr::mutate(Event = stringr::str_remove(Event, " Women$| Men$"))

    #### Pulls out event date ####
    Date_String <- paste(paste0("\\d{1,2}\\s", month.abb, "\\s20\\d{2}"), collapse = "|")

    event_date <- stringr::str_extract(flash_file, Date_String)
    event_date <- event_date[is.na(event_date) == FALSE]
    event_date <- event_date[1] # in case there are birthdates

    if(length(event_date) > 0){
      event_date <- as.Date(event_date, format = "%d %b %Y")
    }

    #### set up strings ####
    Name_String <-
      "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
    Result_String_Spaces <- "\\d{0,2}\\:?\\-?\\d{1,2}\\.\\d{2}m?"
    Result_String <- "^\\d{0,2}\\:?\\-?\\d{1,2}\\.\\d{2}m?$"
    Result_Specials_String <-
      paste0(Result_String, "|^NT$|^NP$|^DQ$|^DNS$|^DNF$|^FOUL$|^NH$|^SCR$|^FS$")
    Wind_String <-
      "\\+\\d\\.\\d|\\-\\d\\.\\d|^NWS$|^NWI$|^\\d\\.\\d$"
    Points_String <- "^\\d\\d?\\.?\\d?$"
    Date_String <- paste(paste0("^\\d{1,2}-", month.abb, "-\\d{4}$"), collapse = "|")
    Age_String <- paste0("^SR$|^JR$|^SO$|^FR$|^M?W?[:digit:]{1,3}$|", Date_String)
    Tiebreaker_String <- "\\d\\.\\d{3}$"

    #### clean input data ####
      suppressWarnings(
        data_1 <- flash_file %>%
          .[purrr::map(., length) > 0] %>%
          .[purrr::map(., stringr::str_length) > 50] %>%
          .[stringr::str_count(., "\\d\\)") < 2] %>%  # remove inline splits and team scores as 1) Alfred 2) Ithaca etc.
          .[stringr::str_detect(., paste0(Result_String_Spaces, "|DQ|DNS|DNF|FOUL|NH|SCR|FS"))] %>% # must Results_String because all results do
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
          stringr::str_replace_all("(?<=\\d{1,2}) (?=Jan |Feb |Mar |Apr |May |Jun |Jul |Aug |Sep |Oct |Nov |Dec )", "-") %>% # need space after month.abb to not roll up e.g. "1 Marie"
          stringr::str_replace_all("(?<=Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) (?=\\d{4})", "-") %>%
          # remove 'A', 'B' etc. relay designators
          stringr::str_replace_all("\U2013", "PA$$ ") %>% # special dash from pole vault in flash results
          stringr::str_replace_all(" \\'[A-Z]\\' ", "  ") %>% # tf specific  - removes relay A, B etc. designators
          stringr::str_replace_all("  [A-Z]  ", "  ") %>%
          stringr::str_replace_all("\\'\\'", "  ") %>%
          stringr::str_remove_all("(?<=\\.\\d{2,3})\\]? +[Q|q] ?(?=\\s)") %>% # tf specific - removes "q" or "Q" sometimes used to designate a qualifying result
          stringr::str_remove_all("(?<=\\s)[J|j](?=\\d)") %>% # tf specific - removes "j" or "J" sometimes used to designate a judged result
          stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
          stringr::str_replace_all(" \\[", "   ") %>% #1/5/21 for ties times
          stringr::str_replace_all("\\] ", "   ") %>% #1/5/21 for ties times
          stringr::str_replace_all(" \\((?=\\d)", "   ") %>% #1/5/21 for ties times
          stringr::str_replace_all("(?<=\\d)\\) ", "   ") %>% #1/5/21 for ties times
          stringr::str_replace_all("\\*", "_") %>%
          stringr::str_replace_all(" \\+", "  \\+") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all(" \\-", "  \\-") %>%  # tf speciifc, for windspeed
          stringr::str_replace_all("#", "  ") %>%  # tf specific, leading pound sign to spaces
          stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # tf specific - split place and name
          stringr::str_replace_all("(?<=\\dm) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split distance and windspeed
          stringr::str_replace_all("(?<=\\d) (?=[\\-|\\+|\\d])", "  ") %>% # tf specific - split time and windspeed
          stringr::str_replace_all("(?<=\\d{3}) (?=[:alpha:])", "  ") %>% # tf specific - split bib number and name
          stringr::str_replace_all("(?<=\\d{2}) (?=[:alpha:])", "  ") %>% # tf specific - split age and team
          stringr::str_replace_all(" FR ", "  FR  ") %>% # tf specific - split age and team
          stringr::str_replace_all(" SO ", "  SO  ") %>% # tf specific - split age and team
          stringr::str_replace_all(" JR ", "  JR  ") %>% # tf specific - split age and team
          stringr::str_replace_all(" SR ", "  SR  ") %>% # tf specific - split age and team
          stringr::str_replace_all(" DNS ", "  DNS  ") %>% # tf specific - split team and result string
          stringr::str_replace_all(" DNF ", "  DNF  ") %>% # tf specific - split team and result string
          stringr::str_replace_all("(?<=[:alpha:])\\. (?=\\d)", "\\.   ") %>% # tf specific - split team and result string
          stringr::str_replace_all("(?<=[:alpha:])\\. (?=[DNS|DNF|NH|FOUL|DQ])", "\\.   ") %>% # tf specific - split team and result string
          stringr::str_replace_all("DNF (?=[:alpha:])", "DNF  ") %>% # tf specific - split place string and name
          stringr::str_replace_all("DNS (?=[:alpha:])", "DNS  ") %>% # tf specific - split place string and name
          stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # tf specific - split name and age
          stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # tf specific - split name and age
          stringr::str_replace_all(" (M\\d{1,3}) ", "  \\1  ") %>% # tf specific - gendered ages M
          stringr::str_replace_all(" (W\\d{1,3}) ", "  \\1  ") %>% # tf specific - gendered ages W
          stringr::str_replace_all("(?<=\\d\\.\\d) (?=\\d{1,2}\\s)", "  ") %>% # tf specific - split off wind and heat number
          stringr::str_replace_all("(?<=\\d)m (?=[:alpha:])", "m   ") %>% # tf specific - separate meters from record indicator "2.05m MR"
          stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>% # tf specific - split off row_numb
          stringr::str_replace_all(" \\., ", "  Period, ") %>% # for names that only have a period, as in some singapore results
          stringr::str_replace_all("([:alpha])(\\.[:alpha:])", "\\1 \\2") %>%
          stringr::str_remove_all("X?X?PA\\$\\$|XXX|XXO| XX | ?XO ?| O | X | XR ") %>%  # remove rounds
          # stringr::str_remove_all("^[A-Z][a-z].{1,}$") %>%
          trimws() %>%
          .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "^[A-Z][a-z].{1,}|^[:upper:]{3}.*Meet|^[A-CEG-MO-Z][:upper:]{1,2}")))]   # remove records
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
      data_length_11 <- data_1[purrr::map(data_1, length) == 11]
      data_length_12 <- data_1[purrr::map(data_1, length) == 12]
      data_length_13 <- data_1[purrr::map(data_1, length) == 13]

      # treatment of DQs
      # suppressWarnings(DQ <-
      #                    data_1[stringr::str_detect(data_1, Time_Score_String, negate = TRUE) == TRUE])
      # DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      # DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]

      #### thirteen variables ####
      if (length(data_length_13) > 0) {
        suppressWarnings(
          df_13 <- data_length_13 %>%
            list_transform() %>%
            dplyr::mutate(Place = V1) %>%
            dplyr::mutate(Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                                        TRUE ~ "NA")) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                stringr::str_detect(V5, Age_String) == TRUE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "PASS|XX") == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3,
                # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V12, Result_Specials_String) == FALSE &
                  stringr::str_detect(V11, Result_Specials_String) == FALSE &
                  stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V12, Result_Specials_String) == FALSE &
                  stringr::str_detect(V11, Result_Specials_String) == FALSE &
                  stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V12, Result_Specials_String) == TRUE ~ V12,
                stringr::str_detect(V11, Result_Specials_String) == TRUE &
                  stringr::str_detect(V12, Result_Specials_String) == FALSE ~ V11,
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                stringr::str_detect(V6, Result_Specials_String) &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
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
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V9, Tiebreaker_String) == TRUE ~ V9,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
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
              Points,
              Tiebreaker,
              "Row_Numb" = V13
            )
        )
      } else {
        df_13 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### twelve variables ####
      if (length(data_length_12) > 0) {
        suppressWarnings(
          df_12 <- data_length_12 %>%
            list_transform() %>%
            dplyr::mutate(Place = V1) %>%
            dplyr::mutate(
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                stringr::str_detect(V5, Age_String) == TRUE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "PASS|XX") == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V11, Result_Specials_String) == FALSE &
                  stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V11, Result_Specials_String) == FALSE &
                  stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V11, Result_Specials_String) == TRUE ~ V11,
                stringr::str_detect(V10, Result_Specials_String) == TRUE &
                  stringr::str_detect(V11, Result_Specials_String) == FALSE ~ V10,
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                stringr::str_detect(V6, Result_Specials_String) &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
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
                                       stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
                                       stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
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
              "Row_Numb" = V12
            )
        )
      } else {
        df_12 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### eleven variables ####
      if (length(data_length_11) > 0) {
        suppressWarnings(
          df_11 <- data_length_11 %>%
            list_transform() %>%
            dplyr::mutate(Place = V1) %>%
            dplyr::mutate(
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                stringr::str_detect(V5, Age_String) == TRUE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == FALSE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "PASS|XX") == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3,
                # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V10, Result_Specials_String) == FALSE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V10, Result_Specials_String) == TRUE ~ V10,
                stringr::str_detect(V9, Result_Specials_String) == TRUE &
                  stringr::str_detect(V10, Result_Specials_String) == FALSE ~ V9,
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                stringr::str_detect(V6, Result_Specials_String) &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
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
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V9, Tiebreaker_String) == TRUE ~ V9,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
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
              Points,
              Tiebreaker,
              'Row_Numb' = V11
            )
        )
      } else {

        df_11 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### ten variables ####
      if (length(data_length_10) > 0) {
        suppressWarnings(
          df_10 <- data_length_10 %>%
            list_transform() %>%
            dplyr::mutate(Place = V1) %>%
            dplyr::mutate(
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                stringr::str_detect(V5, Age_String) == TRUE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == FALSE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "PASS|XX") == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V9, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE &
                stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V9, Result_Specials_String) == TRUE ~ V9,
                stringr::str_detect(V8, Result_Specials_String) == TRUE &
                  stringr::str_detect(V9, Result_Specials_String) == FALSE ~ V8,
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                stringr::str_detect(V6, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V6,
                stringr::str_detect(V6, Result_Specials_String) &
                  stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
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
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V9, Tiebreaker_String) == TRUE ~ V9,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
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
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "--") == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V8, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V8, Points_String) == FALSE & # for points column in decathalon
                stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V8, Result_Specials_String) == TRUE ~ V8,
                # stringr::str_detect(V8, Points_String) == TRUE & # for points column in decathalon
                #   stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7, # for points column in decathalon
                stringr::str_detect(V7, Result_Specials_String) == TRUE &
                  stringr::str_detect(V8, Result_Specials_String) == FALSE ~ V7,
                stringr::str_detect(V4, Result_Specials_String) &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V4,
                stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Heat = dplyr::case_when(
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V7, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V8, Points_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(
                  stringr::str_detect(V8, Points_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
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
            dplyr::mutate(
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Prelims_Result = dplyr::case_when(
                stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V7, Result_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V4, Result_Specials_String) == FALSE &
                  stringr::str_detect(V6, Result_Specials_String) == FALSE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V7, Result_Specials_String) == FALSE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Wind_Speed = dplyr::case_when(
                stringr::str_detect(V5, Wind_String) == TRUE ~ V5,
                stringr::str_detect(V5, Wind_String) == FALSE &
                  stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                stringr::str_detect(V6, Wind_String) == FALSE &
                  stringr::str_detect(V7, Wind_String) == TRUE ~ V7,
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V8, Wind_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Heat = dplyr::case_when(
                stringr::str_detect(V7, Wind_String) == FALSE &
                  stringr::str_detect(V7, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V8, Points_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(
                  stringr::str_detect(V7, Points_String) == TRUE &
                    stringr:: str_detect(V1, "[:alpha:]") == FALSE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
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
              Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                            TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == FALSE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3,
                stringr::str_detect(V3, Age_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                stringr::str_detect(V3, Name_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V5, "[:alpha:]{2,}") == TRUE ~ V5,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals_Result = dplyr::case_when(
                stringr::str_detect(V6, Result_Specials_String) == TRUE ~ V6,
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
                stringr::str_detect(V6, Wind_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(
                  stringr::str_detect(V6, Points_String) &
                    stringr::str_detect(V1, "[:alpha:]") == FALSE ~ V6, # decathalon points
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
                stringr::str_detect(V6, Tiebreaker_String) == TRUE ~ V6,
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
              Points,
              Tiebreaker,
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
                stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V3, Name_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Bib_Number = dplyr::case_when(stringr::str_detect(V2, "^\\d{1,6}$") == TRUE ~ V2,
                                                 TRUE ~ "NA")) %>%
            dplyr::mutate(
              Age = dplyr::case_when(
                stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V3, Result_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == FALSE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V3, Age_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                stringr::str_detect(V3, Name_String) == TRUE &
                  stringr::str_detect(V5, Result_Specials_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{2,}") == TRUE ~ V4,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V3, Result_Specials_String) == FALSE &
                  stringr::str_detect(V5, Result_Specials_String) == FALSE &
                  stringr::str_detect(V5, "[:alpha:]{2,}") == TRUE ~ V5,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE ~ V3, # for results with date of birth instead of age
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(Team == "NA" & Age == "NA"  ~ Name,
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
            dplyr::mutate(
              Points = dplyr::case_when(
                stringr::str_detect(V5, Points_String) &
                  stringr::str_detect(V1, "[:alpha:]") == FALSE ~ V5, # decathalon points
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Tiebreaker = dplyr::case_when(
                stringr::str_detect(V4, Tiebreaker_String) == TRUE ~ V4,
                stringr::str_detect(V5, Tiebreaker_String) == TRUE ~ V5,
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
              Points,
              Tiebreaker,
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
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE &
                  stringr::str_detect(V3, Result_Specials_String) == FALSE ~ V2,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                                                 TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Age_String) == FALSE &
                  # stringr::str_detect(V3, Result_Specials_String) == FALSE &
                  stringr::str_detect(V4, Result_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V4, "\\d") == TRUE &
                  stringr::str_detect(V3, Result_Specials_String) == FALSE &
                  stringr::str_detect(V1, "\\d") == FALSE ~ V3,
                stringr::str_detect(V4, Age_String) == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, "[:alpha:]{2,}") == TRUE &
                stringr::str_detect(V3, Result_Specials_String) == FALSE ~ V3,
                stringr::str_detect(V3, Result_Specials_String) == TRUE ~ V2, # for results with date of birth instead of age
                TRUE ~ "NA"
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
              Age,
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
        flash_data <-
          dplyr::bind_rows(df_13, df_12, df_11, df_10, df_9, df_8, df_7, df_6, df_5, df_4) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb) %>%
          dplyr::mutate(Exhibition = 0) %>%
          dplyr::mutate(DQ = 0) %>%
          ### moved up from below for DQ work 8/20
          dplyr::mutate(DQ = dplyr::case_when(Place == 10000 &
                                              Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                              Finals_Result %in% c("FOUL", "DNF", "NH", "DQ") == TRUE ~ 1,
                                              TRUE ~ DQ)) %>%
          dplyr::na_if(10000) %>%
          { # Name column might or might not exist
            if("Name" %!in% names(.)) dplyr::mutate(., Name = "NA") else . # relay entries don't have a team column
          } %>%
          dplyr::mutate(dplyr::across(
            c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
          )) %>% # remove any "10000"s added in erroneously
          dplyr::mutate(
            Place = as.character(Place),
            Row_Numb = as.numeric(Row_Numb)
          ) %>%
          dplyr::filter(Row_Numb >= Min_Row_Numb) %>%
          #### clean up Tiebreaker, move over reaction times ####
          # dplyr::mutate(Reaction_Time = dplyr::case_when(stringr::str_detect(Tiebreaker, "^0\\.\\d{3}$") == TRUE ~ Tiebreaker,
          #                                                TRUE ~ "NA")) %>%
        { # Tiebreaker column might or might not exist
          if("Tiebreaker" %in% names(.)) dplyr::mutate(., Tiebreaker = dplyr::case_when(stringr::str_detect(Tiebreaker, "^0\\.\\d{3}$") == TRUE ~ "NA",
                                                                           TRUE ~ Tiebreaker)) else .
        } %>%
          dplyr::na_if("NA")
      )

      #### Address Gendered Ages - not sure if needed for flash results ####
      if("Age" %in% names(flash_data)){
      flash_data <- flash_data %>%
        dplyr::mutate(Gender = stringr::str_extract(Age, "^M|^W")) %>%
        dplyr::mutate(Age = dplyr::case_when(
          is.na(Gender) == FALSE ~ stringr::str_remove(Age, Gender),
          TRUE ~ Age
        ))
      }

      #### Address Birthdates ####
      if("Age" %in% names(flash_data)){
        flash_data <- flash_data %>%
          dplyr::mutate(Birthdate = dplyr::case_when(stringr::str_detect(Age, Date_String) == TRUE ~ Age,
                                                     TRUE ~ "NA")) %>%
          dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(Birthdate, "NA") == FALSE ~ "NA",
                                               TRUE ~ Age)) %>%
          dplyr::na_if("NA")
      }

      #### Address Names with "." renamed to "Period" - not sure if needed for flash results ####
      flash_data <- flash_data %>%
        dplyr::mutate(Name = stringr::str_replace(Name, "Period", "\\."))

      #### added in to work with arrange/distinct calls after adding in events ####
      if ("Prelims_Result" %in% names(flash_data) == FALSE) {
        flash_data$Prelims_Result <- NA
      }

      if ("Wind_Speed" %in% names(flash_data) == FALSE) {
        flash_data$Wind_Speed <- NA
      }

      #### add in events based on row number ranges ####
      flash_data  <-
        transform(flash_data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
        dplyr::arrange(Name, Team, is.na(Wind_Speed), is.na(Prelims_Result)) %>% # new 1/1/21 to deal with results presented by heat and as final on same page
        dplyr::distinct(Name, Team, Event, Prelims_Result, Finals_Result, .keep_all = TRUE) %>%  # new 1/1/21 to deal with results presented by heat and as final on same page
        dplyr::arrange(Row_Numb)

      #### Address Blank Teams for non-relay events ####
      if (all(("Team" %in% names(flash_data)) &
              ("Name" %in% names(flash_data)))) {
        flash_data <- flash_data %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              is.na(Name) == TRUE &
                is.na(Team) == FALSE &
                stringr::str_detect(Event, "Relay|relay|MR|Unknown") == FALSE ~ Team,
              TRUE ~ Name
            )
          ) %>%
          dplyr::mutate(Team = dplyr::case_when(
            Name == Team &
              stringr::str_detect(Event, "Relay|relay|MR|Unknown") == FALSE ~ "NA",
            TRUE ~ Team
          )) %>%
          dplyr::na_if("NA")

        flash_data <- flash_data %>% # to remove team scores that can be captured if they are \\d\\.\\d\\d (like in the case of times)
          dplyr::filter(is.na(Team) == FALSE | all(
            is.na(Team) == TRUE &
              stringr::str_detect(Event, "Jump|jump|Pole|pole|Shot|shot|Javelin|javelin|Hammer|hammer|Discus|discus|Unknown") == TRUE &
              stringr::str_detect(Finals_Result, "DNS|DNF|FOUL|DQ|NH|\\dm|\\d\\-\\d{2}") == FALSE
          ) |
            all(
              is.na(Team) == TRUE &
                stringr::str_detect(Event, "Jump|jump|Pole|pole|Shot|shot|Javelin|javelin|Hammer|hammer|Discus|discus|Unknown") == FALSE &
                stringr::str_detect(Finals_Result, "DNS|DNF|FOUL|DQ|NH|^[8-9]\\.\\d\\d$|^1[8-9]\\.\\d\\d$|^[2-9]\\d\\.\\d\\d$|^\\d?\\d\\:\\d\\d\\.\\d\\d$") == FALSE
            ))
      }

      #### adding in rounds ####
      if (flash_rounds == TRUE) {
        rounds_data <- flash_rounds_parse(flash_file)

        # rounds <- rounds %>%
        #   # transform(rounds, Row_Numb_Adjusted = flash_data$Row_Numb[findInterval(Row_Numb, flash_data$Row_Numb)]) %>%
        #   dplyr::select(-Row_Numb)

        if (all(nrow(rounds_data) <  2 & nrow(rounds_data) >  0)) {
          if (min(rounds_data$Row_Numb) < min(flash_data$Row_Numb)) {
            flash_data <-
              cbind(flash_data, rounds_data %>% dplyr::select(-Row_Numb), row.names = NULL)
          }

          } else {
            if(nrow(rounds_data) > nrow(flash_data)){
              rounds_data <- rounds_data[1:nrow(flash_data),] # removes issue with team scores added to bottom of results being caught up as rounds
            }

            rounds_data <-
              transform(rounds_data, Row_Numb_Adjusted = flash_data$Row_Numb[findInterval(Row_Numb, flash_data$Row_Numb)]) %>%
              dplyr::select(-Row_Numb)

            flash_data <-
              dplyr::left_join(flash_data,
                               rounds_data,
                               by = c("Row_Numb" = "Row_Numb_Adjusted"))
          }
      }

      #### adding in rounds attempts ####
      if (flash_round_attempts == TRUE) {
        round_attempts_data <- flash_round_attempts_parse(flash_file)

        if (nrow(round_attempts_data) > 1) {
          round_attempts_data <-
            transform(round_attempts_data, Row_Numb_Adjusted = flash_data$Row_Numb[findInterval(Row_Numb, flash_data$Row_Numb)]) %>%
            dplyr::select(-Row_Numb)

          flash_data <-
            dplyr::left_join(flash_data,
                             round_attempts_data,
                             by = c("Row_Numb" = "Row_Numb_Adjusted"))

        }
      }

      if(flash_split_attempts == TRUE){
        suppressMessages(
        flash_data <- attempts_split(flash_data)
        )
      }

      #### ordering columns after adding rounds ####
      if (all(flash_rounds == TRUE &
              flash_round_attempts == TRUE)) {
        flash_data <- flash_data %>%
          dplyr::select(colnames(.)[stringr::str_detect(names(.), "^Round", negate = TRUE)], sort(colnames(.)[stringr::str_detect(names(.), "^Round")]))
      }

      #### clean up unneeded columns ####
      flash_data <- flash_data %>%
        dplyr::arrange(Row_Numb) %>%
        dplyr::select(which(SwimmeR::`%!in%`(names(.), c("Row_Numb", "Exhibition", "Points", "Heat"))))

      # removes unneeded Attempt_X columns (i.e. those that don't have an associated Attempt_Result)
      if (any(stringr::str_detect(names(flash_data), "Round_\\d{1,}_Attempt")) == TRUE) {
        flash_data <- remove_unneeded_rounds(flash_data) %>%
          dplyr::na_if("")
      }

      # add in date
      flash_data <- flash_data %>%
        dplyr::mutate(Event_Date = event_date)

      #### remove empty columns (all values are NA) ####
      flash_data <- Filter(function(x)
        ! all(is.na(x)), flash_data)

      return(flash_data)
    }
  }
