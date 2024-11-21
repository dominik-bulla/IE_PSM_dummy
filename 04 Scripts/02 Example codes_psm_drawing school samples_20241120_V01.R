### Description ----------------------- ----------------------- ----------------------- -----------------------
# This dummy code presents the schematic steps to draw treatment and control samples from treatment and control sampl frames.
# It is assumed that there are 2 treatment arms as well as 1 control arm.
#
# This file draws the school samples using propensity score matching
#
# Author: Dominik Bulla
# Date: 24/11/2024
# Contact: dominik.bulla@gmail.com



### Environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/IE_PSM_dummy")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())
set.seed(123)



### Load packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)
library(survey)
library(sampling)
library(MatchIt)



### Load dummy data ----------------------- ----------------------- ----------------------- -----------------------

zones <- read.csv("02 processed data/zones_full_data.csv")
district_summary <- read.csv("02 processed data/district_summary.csv")

sample_frame_a1 <- read.csv("02 processed data/school_level_data_treatment1.csv")
sample_frame_a2 <- read.csv("02 processed data/school_level_data_treatment2.csv")
sample_frame_a0 <- read.csv("02 processed data/school_level_data_control.csv")



### Determine district-level sample sizes ----------------------- ----------------------- ----------------------- -----------------------

sample_size <- sample_frame_a1 %>%
  count(district) %>%
  mutate(total = round(n / sum(n) * 36))  # Proportional to district size
sample_size <- sample_size %>%
  mutate(remoteness1 = district_summary$remoteness1[1 : 6]) %>%
  mutate(remoteness1 = round((remoteness1 * total) / 100, 0)) %>%
  mutate(remoteness0 = total - remoteness1) %>%
  mutate(district = as.character(district)) %>%
  select(district, n, total, remoteness0, remoteness1)
total_row <- data.frame(
  district = "Total",
  n = sum(sample_size$n),
  total = sum(sample_size$total),
  remoteness0 = sum(sample_size$remoteness0),
  remoteness1 = sum(sample_size$remoteness1))
sample_size <- rbind(sample_size, total_row)
rm(total_row)



### Draw Tiwoloka school sample for each district stratum  ----------------------- ----------------------- ----------------------- -----------------------

school_sample1 <- strata(
  data = sample_frame_a1,
  stratanames = c("district", "remoteness"),
  size = as.vector(t(sample_size[1:6 ,c("remoteness0", "remoteness1")])),
  method = "systematic",
  pik = sample_frame_a1$standard3
)
school_sample1 <- school_sample1 %>%
  select(ID_unit, Stratum, Prob) %>%
  rename("school_id" = ID_unit)
school_sample1 <- merge(school_sample1, sample_frame_a1, by = "school_id")



### Match up Draw Tiwoloka educational zones with zones pertaining to other arms  ----------------------- ----------------------- ----------------------- -----------------------

# Tiwoloka educational zones

school_sample1_zones <- unique(school_sample1$zone)
zones <- zones %>%
  mutate(sample = 0) %>%
  mutate(sample = ifelse(id %in% school_sample1_zones, 1, 0))

# educational zones pertaining to second treatment

school_sample2_zones <- zones %>%
  filter(treatment2 > 0 | sample == 1)
psm_model_z2 <- matchit(sample ~ urbanization + poverty + primary_schools + enrollment + other_characteristic, 
                        data = school_sample2_zones, 
                        method = "nearest", # Nearest neighbor matching
                        distance = "logit") # Use logistic regression for propensity score
summary(psm_model_z2)

#plot(psm_model, type = "jitter")
#plot(psm_model, type = "qq")

school_sample2_zones <- match.data(psm_model_z2)$id[match.data(psm_model_z2)$sample  == 0]

# educational zones pertaining to control

school_sample0_zones <- zones %>%
  filter(control > 0 | sample == 1) %>%
  filter(!(treatment2 > 0 & sample == 0)) 
psm_model_z0 <- matchit(sample ~ urbanization + poverty + primary_schools + enrollment + other_characteristic, 
                        data = school_sample0_zones, 
                        method = "nearest", # Nearest neighbor matching
                        distance = "logit") # Use logistic regression for propensity score
summary(psm_model_z0)

#plot(psm_model, type = "jitter")
#plot(psm_model, type = "qq")

school_sample0_zones <- match.data(psm_model_z0)$id[match.data(psm_model_z0)$sample  == 0]



### Draw school sample for second treatment arm ----------------------- ----------------------- ----------------------- -----------------------

school_sample2 <- sample_frame_a2 %>%
  filter(zone %in% school_sample2_zones)
school_sample2 <- rbind(school_sample2, school_sample1[, which(colnames(school_sample1) %in% colnames(school_sample2))]) 
school_sample2 <- school_sample2 %>%
  mutate(treatment = ifelse(treatment == 2, 0, treatment))

psm_model_t2 <- matchit(treatment ~ standard3 + remoteness + student_teacher_ratio, 
                        data = school_sample2, 
                        method = "nearest", # Nearest neighbor matching
                        distance = "logit") # Use logistic regression for propensity score
summary(psm_model_t2)

#plot(psm_model, type = "jitter")
#plot(psm_model, type = "qq")

schools <- match.data(psm_model_t2)$school_id[match.data(psm_model_t2)$treatment  == 0]
school_sample2 <- school_sample2 %>%
  filter(school_id %in% schools)

# The previous school matching only used schools from those zones that were matched with treatment 1 zones. 
# In the following, we now directly match treatment-1 and treatment-2 schools.

school_sample2_2 <- rbind(sample_frame_a2, school_sample1[, which(colnames(school_sample1) %in% colnames(school_sample2))]) 
school_sample2_2 <- school_sample2_2 %>%
  mutate(treatment = ifelse(treatment == 2, 0, treatment))

psm_model_t2_2 <- matchit(treatment ~ standard3 + remoteness + student_teacher_ratio, 
                          data = school_sample2_2, 
                          method = "nearest", # Nearest neighbor matching
                          distance = "logit") # Use logistic regression for propensity score
summary(psm_model_t2_2)

schools <- match.data(psm_model_t2_2)$school_id[match.data(psm_model_t2_2)$treatment  == 0]
school_sample2_2 <- school_sample2_2 %>%
  filter(school_id %in% schools)



### Draw school sample for control group ----------------------- ----------------------- ----------------------- -----------------------

school_sample0 <- sample_frame_a0 %>%
  filter(zone %in% school_sample0_zones)
school_sample0 <- rbind(school_sample0, school_sample1[, which(colnames(school_sample1) %in% colnames(school_sample0))]) 

psm_model_c <- matchit(treatment ~ standard3 + remoteness + student_teacher_ratio, 
                       data = school_sample0, 
                       method = "nearest", # Nearest neighbor matching
                       distance = "logit") # Use logistic regression for propensity score
summary(psm_model_c)

#plot(psm_model, type = "jitter")
#plot(psm_model, type = "qq")

schools <- match.data(psm_model_c)$school_id[match.data(psm_model_c)$treatment  == 0]
school_sample0 <- school_sample0 %>%
  filter(school_id %in% schools)

# The previous school matching only used schools from those zones that were matched with treatment 1 zones. 
# In the following, we now directly match treatment-1 and control schools.

school_sample0_2 <- rbind(sample_frame_a0, school_sample1[, which(colnames(school_sample1) %in% colnames(school_sample2))]) 

psm_model_c_2 <- matchit(treatment ~ standard3 + remoteness + student_teacher_ratio, 
                      data = school_sample0_2, 
                      method = "nearest", # Nearest neighbor matching
                      distance = "logit") # Use logistic regression for propensity score
summary(psm_model_c_2)


schools <- match.data(psm_model_c_2)$school_id[match.data(psm_model_c_2)$treatment  == 0]
school_sample0_2 <- school_sample0_2 %>%
  filter(school_id %in% schools)



### Get the matching results cleaned up  ----------------------- ----------------------- ----------------------- -----------------------

psm_model_z2 <- summary(psm_model_z2)
psm_model_z2_all <- psm_model_z2$sum.all
psm_model_z2_matched <- psm_model_z2$sum.matched

psm_model_z0 <- summary(psm_model_z0)
psm_model_z0_all <- psm_model_z0$sum.all
psm_model_z0_matched <- psm_model_z0$sum.matched

psm_model_t2 <- summary(psm_model_t2)
psm_model_t2_all <- psm_model_t2$sum.all
psm_model_t2_matched <- psm_model_t2$sum.matched

psm_model_t2_2 <- summary(psm_model_t2_2)
psm_model_t2_2_all <- psm_model_t2_2$sum.all
psm_model_t2_2_matched <- psm_model_t2_2$sum.matched

psm_model_c <- summary(psm_model_c)
psm_model_c_all <- psm_model_c$sum.all
psm_model_c_matched <- psm_model_c$sum.matched

psm_model_c_2 <- summary(psm_model_c_2)
psm_model_c_2_all <- psm_model_c_2$sum.all
psm_model_c_2_matched <- psm_model_c_2$sum.matched



### Save data  ----------------------- ----------------------- ----------------------- -----------------------

write.csv(school_sample1, "03 Results/school_sample_treatment1_20241121_V01.csv", row.names = FALSE)
write.csv(school_sample2, "03 Results/school_sample_treatment2_20241121_V01.csv", row.names = FALSE)
write.csv(school_sample2_2, "03 Results/school_sample_treatment2_alternative_20241121_V01.csv", row.names = FALSE)
write.csv(school_sample0, "03 Results/school_sample_control_20241121_V01.csv", row.names = FALSE)
write.csv(school_sample0_2, "03 Results/school_sample_control_alternative_20241121_V01.csv", row.names = FALSE)

write.csv(psm_model_z2_all, "03 Results/psm_results_treatment2_zones_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_z2_matched, "03 Results/psm_results_treatment2_zones_matches_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_z0_all, "03 Results/psm_results_control_zones_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_z0_matched, "03 Results/psm_results_control_zones_matched_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_t2_all, "03 Results/psm_results_treatment2_schools_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_t2_matched, "03 Results/psm_results_treatment2_schools_matched_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_t2_2_all, "03 Results/psm_results_treatment2_schools_alternative_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_t2_2_matched, "03 Results/psm_results_treatment2_schools_alternative_matched_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_c_all, "03 Results/psm_results_control_schools_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_c_matched, "03 Results/psm_results_control_schools_matched_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_c_2_all, "03 Results/psm_results_control_schools_alternative_all_20241121_V01.csv", row.names = TRUE)
write.csv(psm_model_c_2_matched, "03 Results/psm_results_control_schools_alternative_matched_20241121_V01.csv", row.names = TRUE)

