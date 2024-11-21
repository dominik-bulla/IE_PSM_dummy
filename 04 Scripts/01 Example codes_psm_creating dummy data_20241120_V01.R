### Description ----------------------- ----------------------- ----------------------- -----------------------
# This dummy code presents the schematic steps to draw treatment and control samples from treatment and control sampl frames.
# It is assumed that there are 2 treatment arms as well as 1 control arm.
#
# This file creates the dummy data
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



### Create secondary data on zones ----------------------- ----------------------- ----------------------- -----------------------

zones <- data.frame(
  id = 1:126,
  district = sample(1:6, 126, replace = TRUE),
  urbanization = sample(c("urban", "rural", "semi rural"), 126, replace = TRUE),
  poverty = round(rnorm(126, mean = 70.1, sd = 10), 2),
  primary_schools = sample(6 : 10, 126, replace = TRUE),
  primary_students = round(rnorm(126, mean = 2890, sd = 250), 0),
  enrollment = round(rnorm(126, mean = 126.44, sd = 25), 2),
  other_characteristic = sample(1 : 6, 126, replace = TRUE)
)
zones <- zones %>%
  mutate(primary_students = primary_students * primary_schools)

165

### Create sample frame (treatment arm 1) ----------------------- ----------------------- ----------------------- -----------------------
zones_a1 <- sample(zones$id, 26)
sample_frame_a1 <- data.frame(
  school_id = 1:81,
  district = sample(1:6, 81, replace = TRUE),
  zone = 0,
  treatment = 1,
  standard3 = as.integer(rnorm(81, mean = 165, sd = 25)),
  distanceM = rnorm(81, mean = 9, sd = 3.1),
  student_teacher_ratio = rnorm(81, mean = 30, sd = 5)
)
for(row in 1: nrow(sample_frame_a1)) {
  sample_frame_a1$zone[row] <- sample(zones$id[zones$district == sample_frame_a1$district[row]][1:9], 1, replace = TRUE)
}


# Create within-district stratification of all sample frames
# For illustrative purposes, the schools will be stratified by remoteness (i.e., the distance from the main road). 
# Remoteness is when distance of a given school is larger than the average distance of Tiwoloka schools. This mean will be 
# aplied to all school sample frames. 

sample_frame_a1 <-  sample_frame_a1 %>%
  mutate(remoteness = ifelse(distanceM < mean(sample_frame_a1$distanceM), 0, 1)) %>%
  select(school_id, district, zone, treatment, standard3, distanceM, remoteness, student_teacher_ratio) %>%
  arrange(district, remoteness, zone, school_id)

# Input school numbers into the dummy dataset on zones

sample_frame_a1_summary <- sample_frame_a1 %>%
  group_by(zone) %>%
  summarise(treatment1 = n(),
            remoteness1 = sum(remoteness)) 
zones <- merge(zones, sample_frame_a1_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a1, sample_frame_a1_summary)



### Create sample frame (treatment arm 2) ----------------------- ----------------------- ----------------------- -----------------------
zones_a2 <- sample(zones$id, 20)
sample_frame_a2 <- data.frame(
  school_id = max(sample_frame_a1$school_id) : (max(sample_frame_a1$school_id) + 69),
  district = sample(1:6, 69, replace = TRUE),
  zone = 0,
  treatment = 2,
  standard3 = as.integer(rnorm(69, mean = 170, sd = 27)),
  distanceM = rnorm(69, mean = 7, sd = 2.8),
  student_teacher_ratio = rnorm(69, mean = 40, sd = 7)
)
row <- 1
count <- 1
for(row in 1: nrow(sample_frame_a2)) {
  if(count == 3) {
    sample_frame_a2$zone[row] <- previous
    count <- 1
  } else {
    sample_frame_a2$zone[row] <- sample(sample(zones$id[zones$district == sample_frame_a2$district[row]])[1 : (length(zones$id[zones$district == sample_frame_a2$district[row]]) - 2)], 1, replace = TRUE)
    count <- count + 1
    previous <- sample_frame_a2$zone[row]
  }
}

# Create within-district stratification of all sample frames
# For illustrative purposes, the schools will be stratified by remoteness (i.e., the distance from the main road). 
# Remoteness is when distance of a given school is larger than the average distance of Tiwoloka schools. This mean will be 
# aplied to all school sample frames. 

sample_frame_a2 <-  sample_frame_a2 %>%
  mutate(remoteness = ifelse(distanceM < mean(sample_frame_a1$distanceM), 0, 1)) %>%
  select(school_id, district, zone, treatment, standard3, distanceM, remoteness, student_teacher_ratio) %>%
  arrange(district, remoteness, zone, school_id)

# Input school numbers into the dummy dataset on zones

sample_frame_a2_summary <- sample_frame_a2 %>%
  group_by(zone) %>%
  summarise(treatment2 = n(),
            remoteness2 = sum(remoteness)) 
zones <- merge(zones, sample_frame_a2_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a2, sample_frame_a2_summary)



### Create sample frame (control arm) ----------------------- ----------------------- ----------------------- -----------------------
zones_a0 <- sample(zones$id, 80)
sample_frame_a0 <- data.frame(
  school_id = max(sample_frame_a2$school_id) : (max(sample_frame_a2$school_id) + 757),
  district = sample(1:6, 757, replace = TRUE),
  zone = 0,
  treatment = 0,
  standard3 = as.integer(rnorm(757, mean = 175, sd = 24)),
  distanceM = rnorm(757, mean = 12, sd = 4.8),
  student_teacher_ratio = rnorm(757, mean = 50, sd = 12)
)
for(row in 1: nrow(sample_frame_a0)) {
  sample_frame_a0$zone[row] <- sample(zones$id[zones$district == sample_frame_a0$district[row]], 1, replace = TRUE)
}

# Create within-district stratification of all sample frames
# For illustrative purposes, the schools will be stratified by remoteness (i.e., the distance from the main road). 
# Remoteness is when distance of a given school is larger than the average distance of Tiwoloka schools. This mean will be 
# aplied to all school sample frames. 

sample_frame_a0 <-  sample_frame_a0 %>%
  mutate(remoteness = ifelse(distanceM < mean(sample_frame_a1$distanceM), 0, 1)) %>%
  select(school_id, district, zone, treatment, standard3, distanceM, remoteness, student_teacher_ratio) %>%
  arrange(district, remoteness, zone, school_id)

# Input school numbers into the dummy dataset on zones

sample_frame_a0_summary <- sample_frame_a0 %>%
  group_by(zone) %>%
  summarise(control = n(),
            remotenessC = sum(remoteness)) 
zones <- merge(zones, sample_frame_a0_summary, by.x = "id", by.y = "zone", all.x = TRUE)
rm(zones_a0, sample_frame_a0_summary, row)



### Check quality of dummy dataset  ----------------------- ----------------------- ----------------------- -----------------------
zones <- zones %>%
  mutate(treatment1 = ifelse(is.na(treatment1), 0, treatment1), 
         treatment2 = ifelse(is.na(treatment2), 0, treatment2),
         control = ifelse(is.na(control), 0, control))

district_summary <- zones %>%
  group_by(district) %>%
  summarise(treatment1 = sum(treatment1),
            remoteness1 = sum(remoteness1, na.rm = TRUE),
            treatment2 = sum(treatment2),
            remoteness2 = sum(remoteness2, na.rm = TRUE),
            control = sum(control),
            remotenessC = sum(remotenessC, na.rm = TRUE)) %>%
  mutate(district = as.character(district))

total_row <- data.frame(
  district = "Total",
  treatment1 = sum(district_summary$treatment1),
  remoteness1 = sum(district_summary$remoteness1),
  treatment2 = sum(district_summary$treatment2),
  remoteness2 = sum(district_summary$remoteness2),
  control = sum(district_summary$control),
  remotenessC = sum(district_summary$remotenessC))
district_summary <- rbind(district_summary, total_row)
rm(total_row)

district_summary <- district_summary %>%
  mutate(remoteness1 = round(remoteness1 / treatment1 *100, 2), 
         remoteness2 = round(remoteness2 / treatment1 *100, 2),
         remotenessC = round(remotenessC / control *100, 2))

# construction of the dummy data is successful



### Save data  ----------------------- ----------------------- ----------------------- -----------------------

write.csv(zones, "02 processed data/zones_full_data.csv", row.names = FALSE)
write.csv(district_summary, "02 processed data/district_summary.csv", row.names = FALSE)
write.csv(sample_frame_a1, "02 processed data/school_level_data_treatment1.csv", row.names = FALSE)
write.csv(sample_frame_a2, "02 processed data/school_level_data_treatment2.csv", row.names = FALSE)
write.csv(sample_frame_a0, "02 processed data/school_level_data_control.csv", row.names = FALSE)

