
prop_test <- function(cut, question, survey_data) {
  
  require(survey)
  require(rlang)
  
  question <- enquo(question)
  cut <- enquo(cut)
  
  formula <- paste("~", question, "+", cut)
  
  formula_chi <- formula[2]
  
  cur1_chi <- svychisq(formula = as.formula(formula_chi), survey_data, Ntotal = 100)
  cur1_augment <- broom::augment(cur1_chi)
  cur1_prop_long <- cur1_augment[, c(1, 2, 6)]
  
  cur1_prop_wide <- reshape2::dcast(data = cur1_prop_long, formula = as.formula(paste(question, "~", cut)[2]), value.var = ".col.prop")
  
  
  
  return(cur1_prop_wide)
  
}

