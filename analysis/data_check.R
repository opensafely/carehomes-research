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

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

sink("./data_check_log.txt", type = "output")

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * tpp_coverage_included.rds
#   - Estimated coverage of TPP per MSOA, including only MSOAs with coverage >=80%

# args <- c("input_clean.csv")
args = commandArgs(trailingOnly = TRUE)

input_clean <- readRDS(args[1]) 

# ---------------------------------------------------------------------------- #

#----------------------------------------#
#      UNIQUENESS OF HOUSEHOLD ID        #
#----------------------------------------#

print("No. households, by household_id alone and by household_ID + MSOA")
input_clean %>%
  summarise(N_hhID = n_distinct(household_id),
            N_msoa_hhID = n_distinct(HHID))

print("Uniqueness of household characteristics over all residents:")
input_clean %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa, na.rm = T), 
            region = n_distinct(region, na.rm = T),
            household_size_tot = n_distinct(household_size_tot, na.rm = T),
            care_home_type = n_distinct(care_home_type, na.rm = T),
            percent_tpp = n_distinct(household_size_tot, na.rm = T),
            imd = n_distinct(imd, na.rm = T),
            rural_urban = n_distinct(rural_urban, na.rm = T)) -> n_distinct_chars

# Should be one distinct value for every household
summary(n_distinct_chars)


print("Uniqueness of household characteristics over care home residents:")
input_clean %>%
  filter(ch_ge65) %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa, na.rm = T), 
            region = n_distinct(region, na.rm = T),
            household_size_tot = n_distinct(household_size_tot, na.rm = T),
            care_home_type = n_distinct(care_home_type, na.rm = T),
            imd = n_distinct(imd, na.rm = T),
            rural_urban = n_distinct(rural_urban, na.rm = T)) %>%
  ungroup() -> n_distinct_chars2

# Should be one distinct value for every household
summary(n_distinct_chars2)

print("No. care homes with non-unique characteristics across residents:")
n_distinct_chars2 %>%
  dplyr::select(-household_id) %>%
  summarise(across(everything(), function(x) sum(x > 1)))


# ---------------------------------------------------------------------------- #

#---------------------------------#
#      CHECK COUNTS BY TYPE       #
#---------------------------------#

# By household type
print("No. households, patients and probable cases per carehome type:")
input_clean %>%
  group_by(care_home_type, age_ge65) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# By institution
print("Possible prisons/institutions (size>20 and not CH)")
input_clean %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# ---------------------------------------------------------------------------- #

#-------------------------------------------------#
#      CHECK TPP COVERAGE WITHIN CARE HOMES       #
#-------------------------------------------------#

print("Care homes registered under > 1 system:")
input_clean %>%
  filter(ch_ge65) %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes with < 100% coverage:")
input_clean %>%
  filter(ch_ge65) %>%
  group_by(percent_tpp < 100) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes % TPP coverage:")
summary(
  input_clean %>%
    filter(ch_ge65) %>%
    dplyr::select(household_id, percent_tpp) %>%
    unique() %>% 
    pull(percent_tpp)
)

print("Care homes % TPP coverage category:")
summary(
  input_clean %>%
    filter(ch_ge65) %>%
    dplyr::select(household_id, percent_tpp) %>%
    unique() %>% 
    mutate(percent_tpp_cat = cut(percent_tpp, 
                                 breaks = 10,
                                 include.lowest = TRUE)) %>%
    pull(percent_tpp_cat)
)


# ---------------------------------------------------------------------------- #

#----------------------------------#
#      CHECK HOUSEHOLD SIZES       #
#----------------------------------#

print("Household size by care home type:")
input_clean %>%
  filter(!is.na(household_size_tot)) %>%
  group_by(care_home_type, age_ge65) %>%
  summarise(mean = mean(household_size_tot),
            sd = sd(household_size_tot),
            median = median(household_size_tot),
            minmax = paste(min(household_size_tot), max(household_size_tot), sep = ", ")) 

print("Number of records by care home type:")
input_clean %>%
  group_by(care_home_type, age_ge65, household_id) %>%
  summarise(n_resid = n()) %>%
  group_by(care_home_type, age_ge65) %>%
  summarise(mean = mean(n_resid),
            sd = sd(n_resid),
            median = median(n_resid),
            minmax = paste(min(n_resid), max(n_resid), sep = ", ")) 


################################################################################

sink()

################################################################################
################################################################################

