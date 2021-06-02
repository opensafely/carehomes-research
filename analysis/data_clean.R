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

options(datatable.old.fread.datetime.character = TRUE)

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
# * tpp_coverage_included.rds
#   - Estimated coverage of TPP per MSOA, including only MSOAs with coverage >=80%

# args <- c("input.csv","tpp_coverage_included.rds", 2)
args = commandArgs(trailingOnly = TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # check for mixed HH/perc TPP agreement
  mutate(perc_tpp_lt100 = (percent_tpp < 100))

# Load MSOA TPP coverage
tpp_cov <- readRDS(args[2])

# MSOA TPP coverage cut off
msoa_cov_cutoff <- args[3]

# Set study period 
study_per <- seq(as.Date("2020-03-01"),as.Date("2020-12-07"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, positive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")
dates <- c(event_dates,"discharge_date")

# ---------------------------------------------------------------------------- #

#----------------------#
#      CLEANING        #
#----------------------#

print("Summary: Raw input")
summary(input_raw)

input <- input_raw %>%
  # Filter just to records from England
  filter(grepl("E",msoa)) %>%
  # Join with MSOA coverage data
  left_join(tpp_cov, by = "msoa") %>% 
  # Set up variables of interest
  mutate(HHID = paste(msoa, household_id, sep = ":"), # Redefine unique household identifier
         # Redefine -1/0 values as NA
         across(c(age, ethnicity, imd, rural_urban), function(x) na_if(x,-1)),
         across(c(imd, household_size), function(x) na_if(x,0)),
         # Variable formatting
         dementia = replace_na(dementia,0),
         ethnicity = as.factor(ethnicity),
         rural_urban = as.factor(rural_urban),
         # Date formats
         across(all_of(dates), ymd),
         # Set all character variables as factor
         across(where(is.character), as.factor),
         # Identify potential prisons/institutions - still needed?
         institution = (care_home_type == "U" & household_size > 20),
         # Define delays
         test_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss),
         prob_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss), 
         # Replace event dates pre 2020 and post end of study as NA
         across(all_of(event_dates), na_replace_dates, max = max(study_per))) %>%
  # Identify individuals with any covid event
  rowwise() %>%
  mutate(case = any(!is.na(c_across(all_of(event_dates))))) %>%
  ungroup() 
  
print("Summary: Cleaned")
summary(input)

# ---------------------------------------------------------------------------- #

# Filter MSOAs by TPP coverage (missing value when merged with included MSOAs in tpp_cov)
exclude <- input %>%
  filter(is.na(tpp_cov_wHHID)) 

print(paste0("Individuals excluded with MSOA ",msoa_cov_cutoff,"% coverage cut off: n = ",nrow(exclude)))
print(paste0("MSOAs excluded with MSOA ",msoa_cov_cutoff,"% coverage cut off: n = ",n_distinct(exclude$msoa)))

input <- input %>%
  filter(!is.na(tpp_cov_wHHID))

# Should now have no records with coverage < cutoff
summary(input$tpp_cov_wHHID)

# Double check
nrow(filter(input, tpp_cov_wHHID < msoa_cov_cutoff))
input <- input %>%
  filter(tpp_cov_wHHID >= msoa_cov_cutoff)

# ---------------------------------------------------------------------------- #
# Check missingness in location/type      

print("Total Patients")
n_distinct(input$patient_id)

print("Patients with missing HH MSOA:")
summary(is.na(input$msoa))

print("Patients with missing HH type:")
summary(is.na(input$care_home_type))

print("HHs with missing MSOA: n = ")
input %>%
  filter(is.na(msoa)) %>%
  pull(HHID) %>%
  n_distinct()

print("HHs with missing type: n = ")
input %>%
  filter(is.na(care_home_type)) %>%
  pull(HHID) %>%
  n_distinct()

print("COVID cases with missing MSOA or HH type: n = ")
input %>%
  filter(is.na(msoa) | is.na(care_home_type)) %>%
  rowwise() %>%
  filter(any(!is.na(c_across(all_of(event_dates))))) %>%
  pull(patient_id) %>%
  n_distinct()

# ---------------------------------------------------------------------------- #
# Drop rows with missing MSOA, household ID or care home type

input <- input %>%
  filter(!is.na(msoa) & household_id > 0 & !is.na(household_id) & !is.na(care_home_type))

# ---------------------------------------------------------------------------- #
# Check uniqueness of household ID

print("No. households, by household_id alone and by household_ID + MSOA")
input %>%
  summarise(N_hhID = n_distinct(household_id),
            N_msoa_hhID = n_distinct(HHID))

print("Uniqueness of household characteristics over residents:")
input %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa), 
            region = n_distinct(region),
            household_size = n_distinct(household_size),
            imd = n_distinct(imd),
            rural_urban = n_distinct(rural_urban)) -> n_distinct_chars

summary(n_distinct_chars)

# ---------------------------------------------------------------------------- #
# Counts of household, individuals and cases  

# By household type
print("No. households, patients and probable cases per carehome type:")
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# By institution
print("Probable prisons/institutions (size>20 and not CH)")
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 20)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# By TPP coverage
print("Care homes registered under > 1 system:")
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes with < 100% coverage:")
input %>%
  filter(care_home_type != "U") %>%
  group_by(percent_tpp < 100) %>% 
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes % TPP coverage:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(HHID, percent_tpp) %>%
  unique() %>% 
  pull(percent_tpp)
)

print("Care homes % TPP coverage category:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(HHID, percent_tpp) %>%
  unique() %>% 
  mutate(percent_tpp_cat = cut(percent_tpp, 
                               breaks = 10,
                               include.lowest = TRUE)) %>%
  pull(percent_tpp_cat)
)

# ---------------------------------------------------------------------------- #
# Check household sizes

print("Household size by care home type:")
input %>%
  filter(!is.na(household_size)) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_size),
            sd = sd(household_size),
            median = median(household_size),
            minmax = paste(min(household_size), max(household_size), sep = ", ")) 

print("Number of records by care home type:")
input %>%
  group_by(care_home_type, HHID) %>%
  summarise(n_resid = n()) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(n_resid),
            sd = sd(n_resid),
            median = median(n_resid),
            minmax = paste(min(n_resid), max(n_resid), sep = ", ")) 

# ---------------------------------------------------------------------------- #

print("Summary: Final")
summary(input)

# Save cleaned input data
saveRDS(input, "./input_clean.rds")
write_csv(input, "./input_clean.csv")

# ---------------------------------------------------------------------------- #

sink()

################################################################################
################################################################################

