### Description ----------------------- ----------------------- ----------------------- -----------------------
# This dummy project demonstrates propensity-score matching (PSM) to analyse observational data.
# The data is real-life data that was collected as part of an assignment given to Dominik Bulla by humanitarian actors.  
#
# This file cleans up the dataset. 
#
# Author: Dominik Bulla
# Date: 21/11/2024
# Contact: dominik.bulla@gmail.com



### Environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/IE_PSM_dummy")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())




### Load packages ----------------------- ----------------------- ----------------------- -----------------------

library(dplyr)
library(psych)
library(fastDummies)
library(openxlsx)



### Import data ----------------------- ----------------------- ----------------------- -----------------------

data <- read.csv("01 Raw data/data.csv")



### Create dummy variables ----------------------- ----------------------- ----------------------- -----------------------

data <- data %>%
  mutate(education = ifelse(education == "Completed formal technical school" | education == "Completed informal technical school", "other", education)) %>%
  dummy_cols(., select_columns = c("IP", "type", "education", "safe", "worried")) %>%
  rename("host" = `type_Host community HH`,
         "idp" = `type_Internally displaced HH`,
         "refugees" = `type_Refugee HH`,
         "edu_secondary" = `education_Completed secondary school`,
         "edu_university" = `education_Completed university or beyond`,
         "edu_primary" = `education_Completed primary school`,
         "edu_no_completion" = `education_Did not complete primary school`,
         "edu_no_education" = `education_Never attended school`,
         "edu_other" = `education_other`, 
         "assafe" = safe_assafe, 
         "lesssafe" = safe_lesssafe, 
         "safer" = safe_safer,
         "asworried" = worried_asworried,
         "lessworried" = worried_lessworried,
         "moreworried" = worried_moreworried) %>%
  mutate(cva = ifelse(cva == 1, "received", "not received")) %>%
  mutate(cva = factor(cva, levels = unique(cva)),
         age2 = scale(age),
         income2 = scale(income)) %>%
  select(sno, IP, IP_A, IP_B, IP_C, IP_D, IP_E, type, host, idp, refugees, age, age2, female, disability, married, children, 
         education, edu_primary, edu_secondary, edu_university, edu_no_completion, edu_no_education, edu_other, 
         cva, safe, assafe, lesssafe, safer, 
         parenting_better, relationship_better, worried, asworried, lessworried, moreworried, income, income2)



### Save data  ----------------------- ----------------------- ----------------------- -----------------------

write.xlsx(data, "02 processed data/data_clean_20240909_V01.xlsx")





