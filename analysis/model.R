# qFIT analysis for Gastro COVID-19 recovery project
# Authors: Nick Kennedy (NikNakk), Gareth Walker

library(tidyverse)
# Read data output from cohortextractor
input <- read_csv("output/input.csv")

# Counts of qFIT by STP
input %>%
  filter(!is.na(qfit)) %>% 
  count(stp) %>% 
  write_csv("output/qfit_by_stp.csv")