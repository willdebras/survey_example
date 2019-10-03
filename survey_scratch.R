#--------------------------------#

# Necessary libraries


library(survey)   # {survey} to set up survey objects
library(srvyr)    # {srvyr} to provide dplyr syntax to survey
library(haven)    # {haven} to add label attributes

# Optional libraries

library(tibble)   #{dplyr} I usually import for a handful of helpful data manipulation functions
library(magrittr) #{magrittr} Adds more pipes!
library(broom)    #{broom} Lets us tidy model outputs


#--------------------------------#

# Read our labeled data via haven

data_full <- read_dta("./harrissurvey2.dta")

#data_full$hhincome <- labelled(data_full$hhincome, c("Less than $10,000" = 1, "$10,000 to $19,999" = 2, "$20,000 to $29,999" =3, "$30,000 to $39,999"=4, "$40,000 to $49,999"=5, "$50,000 to $74,999"=6, "$75,000 to $84,999"=7, "$100,000 to $149,999"=8, "$150,000 or more"=9))


# Generic variant of as.factor to allow application across vectors
data_prepped <- as_factor(data_full)


# Calculate n size
survey_nsize <- as.data.frame(colSums(!is.na(data_prepped))) %>%
  rownames_to_column() %>%
  `colnames<-`(c("question", "nsize")) %>%
  mutate(nsize = prettyNum(nsize, big.mark = ","))

#--------------------------------#

# Explore the metadata that haven provides to us.

attributes(data_prepped$q6)

#Set up a survey object

tl_surv <- data_prepped %>% as_survey_design(ids = su_id, weights = "finalwt")

# Look at some summary statistics

summary(tl_surv)

#--------------------------------#

# Using svytable and srvyr syntax

xtabs(~q6 + raceth, data = data_full)

tib_surv <- svytable(~q6 + raceth, tl_surv, Ntotal = 100) %T>% View() #T pipe returns left side object

    #Note: survey package uses formulas

test_formula <- as.formula("~q6 + raceth")

tib_surv2 <- svytable(test_formula, tl_surv, Ntotal = 100) %T>% View()

# Group_by and survey_mean will provide weighted frequencies

tib <- tl_surv %>%
  group_by(q6) %>%
  summarise(perc = survey_mean(na.rm = TRUE)) %T>% View()

#--------------------------------#

#Running chisquared and using broom to tidy


svychisq(~q6 + marital, tl_surv, Ntotal = 100)

chisq_obj <- svychisq(~q6 + marital, tl_surv, Ntotal = 100)

#Putting the printed output into a table
glance(chisq_obj)

#Putting the returned object into a table
chisq_aug <- augment(chisq_obj)

#Getting proportions

chisq_prop <- chisq_aug[, c(1, 2, 6)]
q6_prop <- dcast(data = chisq_prop, q6 ~ marital, value.var = ".col.prop")


#Running a custom prop test function

test1 <- prop_test(marital, "q6", tl_surv) %T>% View()

#--------------------------------#

# Models :)

#Creating a formula 

harris_formula <- as.formula("q6~marital + raceth + gender")

harris_model1 <- svyolr(harris_formula, tl_surv)

tidy(harris_model1)

#see also svyglm

#--------------------------------#

# Note on replicate weights

# Applying replicate weights with svrepdesign

data(scd)

repweights<-2*cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1), c(0,1,1,0,0,1),
                    c(0,1,0,1,1,0))

scd_replicates<-svrepdesign(data=scd, type="BRR", repweights=repweights, combined.weights=FALSE)

svyratio(~alive, ~arrests, scd_replicates)

#--------------------------------#

# Creating your own replicate weights

#example dataset of academic performance index

data(api)

api_cluster1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

# Adding jackknife weights

api_cl_jackknife <- as.svrepdesign(api_cluster1)

View(api_cl_jackknife$repweights[[1]])

api_cl_bootstrap <- as.svrepdesign(api_cluster1,type="bootstrap", replicates=100)

# Adding balanced repeated replicate weights

data(scd)

scdnofpc <- svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA,
                    nest=TRUE)

scd2brr <- as.svrepdesign(scdnofpc, type="BRR")

# Fay's modified BRR

scd2fay <- as.svrepdesign(scdnofpc, type="Fay",fay.rho=0.3)

#--------------------------------#