stage_parse <- function (text) {
  
  stage_options <- c("Prelim", "Heat \\d", "Semifinal \\d", "Semi \\d", "stage \\d", "Final") %>%
    paste(., collapse = "|")
  
  stages <- text %>%
    .[min(which(stringr::str_detect(., stage_options)))] %>%
    str_extract(., stage_options)
  
  # stages <- text %>%
  #    .[stringr::str_detect(.,
  #                     stage_options)] 
  
  if (length(stages) > 0) {
    stages <- stages %>%
      stringr::str_to_title(.) %>%
      stringr::str_replace(., "Semi ", "Semifinal ") %>%
      stringr::str_trim()
  }
  
  stages <- stages %>%
    as.data.frame() %>%
    separate(., col = ".", into = c("Stage", "Stage_Number"), sep = " ")
  
  return(stages)
}