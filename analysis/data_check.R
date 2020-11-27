################################################################################
# Description: Script to produce initial checks and summaries of raw input data
#
# input: individual patient GP record data extracted from OpenSAFELY according
# to "./analyis/study_definition.py".
#
# Author: Emily S Nightingale
# Date: 01/10/2020
#
################################################################################

################################################################################

library(tidyverse)
library(data.table)
library(dtplyr)

sink("./data_checks.txt")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# Identify vars containing event dates
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")

# args = commandArgs(trailingOnly=TRUE)
# args <- c("./output/input.csv","tpp_msoa_coverage.rds")
args = commandArgs(trailingOnly=TRUE)

## Load shapefiles
# msoa_shp <- readRDS(args[2]) 

tpp_cov <- readRDS(args[2])

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  left_join(tpp_cov, by = "msoa") %>% 
  mutate(across(where(is.character), as.factor)) %>%
  rowwise() %>%
  mutate(case = any(!is.na(c_across(event_dates))))

# ---------------------------------------------------------------------------- #

hh_miss_msoa <- input %>%
  filter(is.na(msoa)) %>%
  pull(household_id) %>%
  n_distinct()

pat_miss_msoa <- input %>%
  filter(is.na(msoa)) %>%
  nrow()

hh_miss_type <- input %>%
  filter(is.na(care_home_type)) %>%
  pull(household_id) %>%
  n_distinct()

pat_miss_type <- input %>%
  filter(is.na(care_home_type)) %>%
  nrow()

pat_cov_miss <- input %>%
  filter((is.na(msoa) | is.na(care_home_type))) %>%
  rowwise() %>%
  filter(any(!is.na(c_across(event_dates)))) %>%
  nrow()

print("HHs with missing MSOA: n = ")
hh_miss_msoa

print("Patients with missing HH MSOA: n = ")
pat_miss_msoa

print("HHs with missing type: n = ")
hh_miss_type

print("Patients with missing HH type: n = ")
pat_miss_type

print("COVID cases with missing MSOA or HH type: n = ")
pat_cov_miss


print("No. households, patients and probable cases per carehome type:")
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n(),
            n_case = sum(case, na.rm = TRUE)) 

print("Probable prisons/institutions (size>15 and not CH)")
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n(),
            n_case = sum(case, na.rm = TRUE)) 

# TPP coverage by MSOA
png("./tpp_coverage_msoa.png", height = 800, width = 800)
input %>%
  group_by(msoa) %>%
  summarise(tpp_cov = unique(tpp_cov)) %>%
  ggplot(aes(tpp_cov)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off()

# png("./tpp_coverage_map.png", height = 800, width = 800)
# input %>%
#   group_by(msoa) %>%
#   summarise(tpp_cov = unique(tpp_cov)) %>%
#   full_join(msoa_shp, by = c("msoa" = "MSOA11CD")) %>%
#   ggplot(aes(geometry = geometry, fill = tpp_cov)) +
#   geom_sf(lwd = 0) + 
#   scale_fill_gradient2() +
#   theme_minimal()
# dev.off() 

print("Care homes registered under > 1 system:")
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n(),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes % TPP coverage:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(household_id, percent_tpp) %>%
  unique() %>% 
  pull(percent_tpp)
)

print("Care homes % TPP coverage category:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(household_id, percent_tpp) %>%
  unique() %>% 
  mutate(percent_tpp_cat = cut(percent_tpp, 5)) %>%
  pull(percent_tpp_cat)
)

input %>%
  filter(care_home_type != "U") %>%
  mutate(percent_tpp_cat = cut(percent_tpp, 5)) %>%
  group_by(percent_tpp_cat) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n(),
            n_case = sum(case, na.rm = TRUE))

png("./tpp_coverage_carehomes.png", height = 800, width = 800)
input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(household_id, percent_tpp) %>%
  unique() %>% 
  ggplot(aes(percent_tpp)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off() 


sink()

