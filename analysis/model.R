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
    across(c(qfit, age, fobt_num), as.numeric),
    qfit_pos = qfit > 10
  )

# Counts of qFIT by region
input %>%
  group_by(region) %>% 
  summarise(
    n_people = n(),
    age_overall = med_iqr(age),
    female = sum(sex == "F"),
    had_qfit = sum(!is.na(qfit_date)),
    qfit_result = med_iqr(qfit[!is.na(qfit_date)]),
    qfit_gt_10 = sum(qfit > 10, na.rm = TRUE),
    had_ft_colorectal_referral = sum(ft_referral, na.rm = TRUE),
    had_ft_colorectal_clinic = sum(ft_clinic, na.rm = TRUE),
    had_colonoscopy = sum(!is.na(colonoscopy_sus)),
    had_flexi_sig = sum(!is.na(flexi_sig_sus)),
    had_crc_diagnosis = sum(crc_diagnosis, na.rm = TRUE),
    referral_if_qfit_gt10 = sum(!is.na(ft_referral_date[qfit > 10 & !is.na(qfit_date)])),
    referral_if_qfit_le10 = sum(!is.na(ft_referral_date[qfit <= 10 & !is.na(qfit_date)])),
    referral_if_qfit_not_done = sum(!is.na(ft_referral_date[is.na(qfit_date)])),
    qfit_referral_timing = med_iqr((qfit_date %--% ft_referral_date) / ddays(1), na.rm = TRUE),
    qfit_clinic_timing = med_iqr((qfit_date %--% ft_clinic_date) / ddays(1), na.rm = TRUE),
    qfit_colonoscopy_timing = med_iqr((qfit_date %--% colonoscopy_sus) / ddays(1), na.rm = TRUE),
    qfit_flexi_sig_timing = med_iqr((qfit_date %--% flexi_sig_sus) / ddays(1), na.rm = TRUE),
    qfit_diagnosis_timing = med_iqr((qfit_date %--% crc_diagnosis_date) / ddays(1), na.rm = TRUE),
    qfit_age = med_iqr(age[!is.na(qfit_date)]),
    qfit_female = sum(sex[!is.na(qfit_date)] == "F"),
    top_four_fit_values = table(round(qfit[!is.na(qfit_date)], 0)) %>%
      sort(decreasing = TRUE) %>%
      head(4) %>% 
      sprintf("%s:%d", names(.), .) %>%
      paste(collapse = "; ")
  ) %>% 
  write_csv("output/qfit_by_region.csv")

qfits_by_region <- input %>% 
  filter(!is.na(qfit_date)) %>% 
  count(region, qfit = round(qfit)) %>%
  ggplot(aes(qfit, n)) +
  geom_col(fill = "dark blue") +
  facet_wrap(quos(region), scales = "free")

ggsave("output/qfits_by_region.png", qfits_by_region, width = 24, height = 24)

qfits_by_region_near_10 <- input %>% 
  filter(!is.na(qfit_date)) %>% 
  count(region, qfit = round(qfit)) %>%
  ggplot(aes(qfit, n)) +
  geom_col(fill = "dark blue") +
  xlim(c(5, 15)) +
  facet_wrap(quos(region), scales = "free")

ggsave("output/qfits_by_region_near_10.png", qfits_by_region_near_10, width = 24, height = 24)

qfit_months_by_region <- input %>% 
  filter(!is.na(qfit_date)) %>% 
  count(region, qfit_month = floor_date(qfit_date, "months")) %>% 
  ggplot(aes(qfit_month, n)) +
  geom_col(fill = "dark blue") +
  facet_wrap(quos(region), scales = "free")

ggsave("output/qfit_months_by_region.png", qfit_months_by_region, width = 24, height = 24)

fobts_by_region <- input %>% 
  filter(!is.na(fobt_num_date)) %>% 
  count(region, fobt_num = round(fobt_num)) %>%
  ggplot(aes(fobt_num, n)) +
  geom_col(fill = "dark blue") +
  facet_wrap(quos(region), scales = "free")

ggsave("output/fobts_by_region.png", fobts_by_region, width = 24, height = 24)

fobt_cat <- input %>% 
  filter(!is.na(fobt_date)) %>% 
  count(region, fobt) %>%
  ggplot(aes(fobt, n)) +
  geom_col(fill = "dark blue") +
  facet_wrap(quos(region), scales = "free") +
  coord_flip()

ggsave("output/fobts_by_region_cat.png", fobt_cat, width = 24, height = 24)

fobt_fit_cat <- input %>% 
  filter(!is.na(fobt_date)) %>% 
  count(region, has_fit = coalesce(fobt_date == qfit_date, FALSE), fobt) %>%
  ggplot(aes(fobt, n, fill = has_fit)) +
  geom_col(position = "stack") +
  facet_wrap(quos(region), scales = "free") +
  coord_flip()

ggsave("output/fobts_by_region_cat_fit.png", fobt_fit_cat, width = 24, height = 24)

# write_csv(as.data.frame(table(input$qfit)), "output/summary_qfit_values.csv")