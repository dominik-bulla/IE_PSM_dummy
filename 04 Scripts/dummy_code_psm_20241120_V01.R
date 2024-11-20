### Description ----------------------- ----------------------- ----------------------- -----------------------
# This dummy code presents the schematic steps to draw treatment and control samples from treatment and control sampl frames.
# It is assumed that there are 2 treatment arms as well as 1 control arm
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



### Create secondary data on zones ----------------------- ----------------------- ----------------------- -----------------------

zones <- data.frame(
  id = 1:126,
  district = sample(1:6, 126, replace = TRUE),
  urbanization = sample(c("urban", "rural", "semi rural"), 126, replace = TRUE),
  poverty = round(rnorm(126, mean = 70.1, sd = 15), 2),
  primary_schools = sample(6 : 10, 126, replace = TRUE),
  primary_students = round(rnorm(126, mean = 2890, sd = 555), 0),
  enrollment = round(rnorm(126, mean = 126.44, sd = 35), 2),
  other_characteristic = sample(1 : 6, 126, replace = TRUE)
)
zones <- zones %>%
  mutate(primary_students = primary_students * primary_schools)



### Create sample frame (treatment arm 1) ----------------------- ----------------------- ----------------------- -----------------------
zones_a1 <- sample(zones$id, 26)
sample_frame_a1 <- data.frame(
  school_id = 1:81,
  district = sample(1:6, 81, replace = TRUE),
  zone = 0,
  treatment = 1,
  school_size = rnorm(81, mean = 3000, sd = 600),
  distance = rnorm(81, mean = 9, sd = 3.1),
  student_teacher_ratio = rnorm(81, mean = 30, sd = 5)
)
for(row in 1: nrow(sample_frame_a1)) {
  sample_frame_a1$zone[row] <- sample(zones$id[zones$district == sample_frame_a1$district[row]], 1, replace = TRUE)
}
sample_frame_a1_summary <- sample_frame_a1 %>%
  group_by(zone) %>%
  summarise(treatment1 = n())
zones <- merge(zones, sample_frame_a1_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a1, sample_frame_a1_summary)



### Create sample frame (treatment arm 2) ----------------------- ----------------------- ----------------------- -----------------------
zones_a2 <- sample(zones$id, 20)
sample_frame_a2 <- data.frame(
  school_id = 1:69,
  district = sample(1:6, 69, replace = TRUE),
  zone = 0,
  treatment = 2,
  school_size = rnorm(69, mean = 600, sd = 125),
  distance = rnorm(69, mean = 7, sd = 2.8),
  student_teacher_ratio = rnorm(69, mean = 40, sd = 7)
)
for(row in 1: nrow(sample_frame_a2)) {
  sample_frame_a2$zone[row] <- sample(zones$id[zones$district == sample_frame_a2$district[row]], 1, replace = TRUE)
}
sample_frame_a2_summary <- sample_frame_a2 %>%
  group_by(zone) %>%
  summarise(treatment2 = n())
zones <- merge(zones, sample_frame_a2_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a2, sample_frame_a2_summary)



### Create sample frame (control arm) ----------------------- ----------------------- ----------------------- -----------------------
zones_a0 <- sample(zones$id, 80)
sample_frame_a0 <- data.frame(
  school_id = 1:757,
  district = sample(1:6, 757, replace = TRUE),
  zone = 0,
  treatment = 0,
  school_size = rnorm(757, mean = 400, sd = 85),
  distance = rnorm(757, mean = 12, sd = 4.8),
  student_teacher_ratio = rnorm(757, mean = 50, sd = 12)
)
for(row in 1: nrow(sample_frame_a0)) {
  sample_frame_a0$zone[row] <- sample(zones$id[zones$district == sample_frame_a0$district[row]], 1, replace = TRUE)
}
sample_frame_a0_summary <- sample_frame_a0 %>%
  group_by(zone) %>%
  summarise(control = n())
zones <- merge(zones, sample_frame_a0_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a0, sample_frame_a0_summary, row)



### Check quality of dummy dataset  ----------------------- ----------------------- ----------------------- -----------------------
zones <- zones %>%
  mutate(treatment1 = ifelse(is.na(treatment1), 0, treatment1), 
         treatment2 = ifelse(is.na(treatment2), 0, treatment2),
         control = ifelse(is.na(control), 0, control))

zones_summary <- zones %>%
  group_by(district) %>%
  summarise(treatment1 = sum(treatment1),
         treatment2 = sum(treatment2),
         control = sum(control)) %>%
  mutate(district = as.character(district))

total_row <- data.frame(
  district = "Total",
  treatment1 = sum(zones_summary$treatment1),
  treatment2 = sum(zones_summary$treatment2),
  control = sum(zones_summary$control))
zones_summary <- rbind(zones_summary, total_row)
rm(total_row)
# construction of the dummy data is successful



### Save data  ----------------------- ----------------------- ----------------------- -----------------------

write.csv(sample_frame_a1, "02 processed data/school_level_data_treatment1.csv")
write.csv(sample_frame_a2, "02 processed data/school_level_data_treatment2.csv")
write.csv(sample_frame_a0, "02 processed data/school_level_data_control.csv")

 

sampling_frame <- data.frame(
  school_id = 1:300,                          # 300 schools
  district_id = sample(1:6, 300, replace = TRUE)  # 6 districts
)

# Step 2: Determine sample sizes for each district
# Proportional allocation
district_counts <- sampling_frame %>%
  count(district_id) %>%
  mutate(sample_size = round(n / sum(n) * 81))  # Proportional to district size


