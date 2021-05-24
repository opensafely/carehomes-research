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

sink("./data_clean_log.txt", type = "output")

options(datatable.old.fread.datetime.character=TRUE)

# Replace dates outside specified range with NAs (default outside 2020)
na_replace_dates <- function(x, min = '2020-01-01', max = '2020-12-31') {
  x[x < min] <- NA
  x[x > max] <- NA
  return(ymd(x))
}

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args <- c("input.csv","tpp_msoa_coverage.rds", 2)
args = commandArgs(trailingOnly = TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # check for mixed HH/perc TPP agreement
  mutate(perc_tpp_lt100 = (percent_tpp < 100))

## Load MSOA TPP coverage
tpp_cov <- readRDS(args[2])

## MSOA TPP coverage cut off
msoa_cov_cutoff <- args[3]

# Set study period 
study_per <- seq(as.Date("2020-03-01"),as.Date("2020-12-07"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, postitive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")
dates <- c(event_dates,"discharge_date")

# ---------------------------------------------------------------------------- #

#----------------------#
#      CLEANING        #
#----------------------#


input <- input_raw %>%
  # Filter just to records from England with non-missing household ID
  filter(grepl("E",msoa) & !is.na(msoa) & household_id > 0) %>%
  # Join with MSOA coverage data
  left_join(tpp_cov, by = "msoa") %>% 
  rowwise() %>%
  # Identify individuals with any covid event
  mutate(case = any(!is.na(c_across(all_of(event_dates))))) %>%
  ungroup() %>%
  # Set up var formats
  mutate(# Redefine -1/0 values as na
         across(c(age, ethnicity, imd, rural_urban), function(x) na_if(x,-1)),
         across(c(imd, household_size), function(x) na_if(x,0)),
         # Variable formatting
         dementia = replace_na(dementia,0),
         ethnicity = as.factor(ethnicity),
         rural_urban = as.factor(rural_urban),
         across(all_of(dates), ymd),
         across(where(is.character), as.factor),
         # Identify potential prisons/institutions
         institution = (care_home_type == "U" & household_size > 20),
         # Define delay vars
         test_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss),
         prob_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss), 
         # replace event dates pre 2020 and post end of study as na
         across(all_of(event_dates), na_replace_dates, max = max(study_per)))

# ---------------------------------------------------------------------------- #

print("Summary: Raw input")
summary(input_raw)

print("Summary: Cleaned input")
summary(input)

# ---------------------------------------------------------------------------- #

# Filter MSOAs by TPP coverage
exclude_msoa <- input %>%
  filter(tpp_cov_wHHID < msoa_cov_cutoff) %>%
  pull(msoa) %>%
  unique()

print(paste0("MSOAs excluded with ",msoa_cov_cutoff,"% coverage cut off: n = ",length(exclude_msoa)))

input <- filter(input, !msoa %in% exclude_msoa)

# ---------------------------------------------------------------------------- #

# Drop rows with missing MSOA or care home type
input <- input %>%
  filter(!is.na(msoa) & !is.na(care_home_type))

# Save cleaned input data
saveRDS(input, "./input_clean.rds")
write_csv(input, "./input_clean.csv")

# ---------------------------------------------------------------------------- #

#----------------------#
#    CHECK COUNTS      #
#----------------------#

print("Total Patients")
n_distinct(input$patient_id)

print("Patients with missing HH MSOA:")
summary(is.na(input$msoa))

print("Patients with missing HH type:")
summary(is.na(input$care_home_type))

print("HHs with missing MSOA: n = ")
input %>%
  filter(is.na(msoa)) %>%
  pull(household_id) %>%
  n_distinct()

print("HHs with missing type: n = ")
input %>%
  filter(is.na(care_home_type)) %>%
  pull(household_id) %>%
  n_distinct()

print("COVID cases with missing MSOA or HH type: n = ")
input %>%
  filter(is.na(msoa) | is.na(care_home_type)) %>%
  rowwise() %>%
  filter(any(!is.na(c_across(all_of(event_dates))))) %>%
  pull(patient_id) %>%
  n_distinct()

print("No. households, patients and probable cases per carehome type:")
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Probable prisons/institutions (size>15 and not CH)")
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes registered under > 1 system:")
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes with < 100% coverage:")
input %>%
  filter(care_home_type != "U") %>%
  group_by(percent_tpp < 100) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes % TPP coverage:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(msoa, household_id, percent_tpp) %>%
  unique() %>% 
  pull(percent_tpp)
)

print("Care homes % TPP coverage category:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(msoa, household_id, percent_tpp) %>%
  unique() %>% 
  mutate(percent_tpp_cat = cut(percent_tpp, 
                               breaks = 10,
                               include.lowest = TRUE)) %>%
  pull(percent_tpp_cat)
)

# input %>%
#   filter(care_home_type != "U") %>%
#   mutate(percent_tpp_cat = cut(percent_tpp, 5)) %>%
#   group_by(percent_tpp_cat) %>%
#   summarise(n_hh = n_distinct(household_id),
#             n_pat = n_distinct(patient_id),
#             n_case = sum(case, na.rm = TRUE))


print("Care home residents test-diagnosis delay")
summary(
input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(prob_death_delay, test_death_delay) 
)

# ---------------------------------------------------------------------------- #

sink()

################################################################################

