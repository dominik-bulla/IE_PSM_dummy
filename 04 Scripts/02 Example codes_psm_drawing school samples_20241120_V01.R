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



sample_frame_a1 <- data.frame(
  school_id = 1:81,
  zone = sample(zones_a1, 81, replace = TRUE),
  treatment = 1,
  school_size = rnorm(81, mean = 3000, sd = 600),
  distance = rnorm(81, mean = 9, sd = 3.1),
  student_teacher_ratio = rnorm(81, mean = 30, sd = 5)
)





sampling_frame <- data.frame(
  school_id = 1:300,                          # 300 schools
  district_id = sample(1:6, 300, replace = TRUE)  # 6 districts
)

# Step 2: Determine sample sizes for each district
# Proportional allocation
district_counts <- sampling_frame %>%
  count(district_id) %>%
  mutate(sample_size = round(n / sum(n) * 81))  # Proportional to district size


