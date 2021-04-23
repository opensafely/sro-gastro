# qFIT analysis for Gastro COVID-19 recovery project
# Authors: Nick Kennedy (NikNakk), Gareth Walker

library(tidyverse)
library(lubridate)
source("analysis/sro_gastro_functions.R")
# Read data output from cohortextractor
input <- read_csv("output/input.csv", col_types = cols(.default = col_character())) %>% 
  mutate(
    across(matches("^(qfit|ft.*|crc.*)_date$"), lubridate::ymd),
    across(c(ft_referral, ft_clinic, crc_diagnosis), ~as.logical(as.integer(.x))),
    across(c(qfit, age), as.numeric)
  )

# Counts of qFIT by STP
input %>%
  group_by(stp) %>% 
  summarise(
    n_people = n(),
    age_overall = med_iqr(age),
    female = sum(sex == "F"),
    had_qfit = sum(!is.na(qfit_date)),
    qfit_result = med_iqr(qfit[!is.na(qfit_date)]),
    had_ft_colorectal_referral = sum(!is.na(ft_referral_date)),
    had_ft_colorectal_clinic = sum(!is.na(ft_clinic_date)),
    had_ft_colorectal_clinic2 = sum(na.omit(ft_clinic)),
    had_crc_diagnosis = sum(!is.na(crc_diagnosis_date)),
    had_crc_diagnosis2 = sum(na.omit(crc_diagnosis)),
    referral_if_qfit_ge10 = sum(!is.na(ft_referral_date[qfit >= 10 & !is.na(qfit_date)])),
    referral_if_qfit_lt10 = sum(!is.na(ft_referral_date[qfit < 10 & !is.na(qfit_date)])),
    referral_if_qfit_not_done = sum(!is.na(ft_referral_date[is.na(qfit_date)])),
    qfit_referral_timing = med_iqr((qfit_date %--% ft_referral_date) / ddays(1), na.rm = TRUE),
    qfit_clinic_timing = med_iqr((qfit_date %--% ft_clinic_date) / ddays(1), na.rm = TRUE),
    qfit_diagnosis_timing = med_iqr((qfit_date %--% crc_diagnosis_date) / ddays(1), na.rm = TRUE),
    qfit_age = med_iqr(age[!is.na(qfit_date)]),
    qfit_female = sum(sex[!is.na(qfit_date)] == "F"),
    top_four_fit_values = table(round(qfit[!is.na(qfit_date)], 0)) %>%
      sort(decreasing = TRUE) %>%
      head(4) %>% 
      sprintf("%s:%d", names(.), .) %>%
      paste(collapse = "; ")
  ) %>% 
  write_csv("output/qfit_by_stp.csv")

# write_csv(as.data.frame(table(input$qfit)), "output/summary_qfit_values.csv")