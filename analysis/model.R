# qFIT analysis for Gastro COVID-19 recovery project
# Authors: Nick Kennedy (NikNakk), Gareth Walker

library(tidyverse)
library(lubridate)
source("sro_gastro_functions.R")
# Read data output from cohortextractor
input <- read_csv("output/input.csv")

# Counts of qFIT by STP
input %>%
  group_by(stp) %>% 
  summarise(
    n_people = n(),
    age_overall = med_iqr(age),
    female = sum(sex == "F"),
    had_qfit = sum(!is.na(qfit_date)),
    had_ft_colorectal_referral = sum(!is.na(ft_referral_date)),
    had_ft_colorectal_clinic = sum(!is.na(ft_clinic_date)),
    had_crc_diagnosis = sum(!is.na(crc_diagnosis_date)),
    qfit_referral_timing = med_iqr((qfit_date %--% ft_referral_date) / ddays(1), na.rm = TRUE),
    qfit_clinic_timing = med_iqr((qfit_date %--% ft_clinic_date) / ddays(1), na.rm = TRUE),
    qfit_diagnosis_timing = med_iqr((qfit_date %--% crc_diagnosis_date) / ddays(1), na.rm = TRUE),
    qfit_age = med_iqr(age[!is.na(qfit_date)]),
    qfit_female = sum(sex[!is.na(qfit_date)] == "F")
  ) %>% 
  write_csv("output/qfit_by_stp.csv")

write_csv(as.data.frame(table(input$qfit)), "output/summary_qfit_values.csv")