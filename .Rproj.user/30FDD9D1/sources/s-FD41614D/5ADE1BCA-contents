survey_setup <- function(data, caseids, weights) {
  
  tl_surv <<- data %>%
    as_survey_design(ids = caseids, weights = weights)
  
  
  survey_nsize <<- as.data.frame(colSums(!is.na(data))) %>%
    rownames_to_column() %>%
    rename(ncount = 2) %>%
    mutate(ncount = prettyNum(ncount, big.mark = ","))
  
  
}