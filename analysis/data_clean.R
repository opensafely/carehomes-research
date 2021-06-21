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


replace_na_neg1 <- function(x) na_if(x,-1)

# Replace dates outside specified range with NAs (default outside 2020)
na_replace_dates <- function(x, min = '2020-01-01', max = '2020-12-31') {
  x <- lubridate::ymd(x)
  x[x < min] <- NA
  x[x > max] <- NA
  return(x)
}


replace_na_neg1 <- function(x) na_if(x,-1)


# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * tpp_coverage_included.rds
#   - Estimated coverage of TPP per MSOA, including only MSOAs with coverage >=80%

# args <- c("input.csv","tpp_coverage_included.rds", 600)
args = commandArgs(trailingOnly = TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # Filter just to records from England
  filter(grepl("E",msoa)) %>%
  # check for mixed HH/perc TPP agreement
  mutate(perc_tpp_lt100 = (percent_tpp < 100))

# Load TPP coverage for included MSOAs
tpp_cov_incl <- readRDS(args[2])

# MSOA TPP coverage cut off
msoa_cov_cutoff <- as.numeric(args[3])

# Set study period 
study_per <- seq(as.Date("2020-03-01"),as.Date("2020-12-07"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, positive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")

# ---------------------------------------------------------------------------- #

print("Summary: Raw input")
summary(input_raw)

print("Summary: TPP coverage, included MSOAs")
summary(tpp_cov_incl)

# ---------------------------------------------------------------------------- #

#----------------------------------------#
#      CHECK MISSING MSOA/HH/TYPE        #
#----------------------------------------#

print("Total Patients")
n_distinct(input_raw$patient_id)

print("Patients with missing HH MSOA:")
summary(is.na(input_raw$msoa))

print("Patients with missing HH type:")
summary(is.na(input_raw$care_home_type))

# Drop individuals with missing household_ID
print(paste0("Dropping patients with missing household_id: n = ", sum(input_raw$household_id == 0)))
input_wHH <- filter(input_raw, household_id != 0)


# Summarise households/patients with missing MSOA/type

print("HHs with missing MSOA: n = ")
input_wHH %>%
  filter(is.na(msoa)) %>%
  pull(household_id) %>%
  n_distinct()

print("HHs with missing type: n = ")
input_wHH %>%
  filter(is.na(care_home_type)) %>%
  pull(household_id) %>%
  n_distinct()

print("COVID cases with missing MSOA or HH type: n = ")
input_wHH %>%
  filter(is.na(msoa) | is.na(care_home_type)) %>%
  rowwise() %>%
  filter(any(!is.na(c_across(all_of(event_dates))))) %>%
  pull(patient_id) %>%
  n_distinct()


# Drop individuals with missing MSOA or care home type
input_nomiss <- input_wHH %>%
  filter(!is.na(msoa) & !is.na(care_home_type)) 
  
# ---------------------------------------------------------------------------- #

#------------------------------------------#
#      EXCLUDE ON MSOA TPP COVERAGE        #
#------------------------------------------#

# Join with MSOA coverage data
input_wcov <- input_nomiss %>%
  left_join(tpp_cov_incl, by = "msoa") 

# Identify MSOAs with missing value when merged with included MSOAs in tpp_cov
exclude <- input_wcov %>%
  filter(is.na(tpp_cov_wHHID)) 

print(paste0("Individuals excluded with MSOA ",msoa_cov_cutoff,"% coverage cut off: n = ",nrow(exclude)))
print(paste0("MSOAs excluded with MSOA ",msoa_cov_cutoff,"% coverage cut off: n = ",n_distinct(exclude$msoa)))

# Drop individuals in MSOAs that don't appear in tpp_cov
input_wcov <- input_wcov %>%
  filter(!(msoa %in% exclude$msoa))

# Should now have no records with coverage < cutoff
print("Summary: Remaining MSOA coverage:")
summary(input_wcov$tpp_cov_wHHID)

# Double check
nrow(filter(input_wcov, tpp_cov_wHHID < msoa_cov_cutoff))

# ---------------------------------------------------------------------------- #

#-----------------------------#
#      VARIABLE SET UP        #
#-----------------------------#

# Set up variables of interest
input_clean <- input_wcov %>%
  lazy_dt() %>%
  mutate(
    
    # Identify dates
    primary_care_case_probable = na_replace_dates(primary_care_case_probable, max = max(study_per)),
    first_pos_test_sgss = na_replace_dates(first_pos_test_sgss, max = max(study_per)),
    covid_admission_date = na_replace_dates(covid_admission_date, max = max(study_per)),
    ons_covid_death_date = na_replace_dates(ons_covid_death_date, max = max(study_per)),
    discharge_date = na_replace_dates(discharge_date, max = max(study_per)),
    
    # Reformat existing variables
    care_home_type = as.factor(care_home_type),
    household_size = na_if(household_size,0),
    region = as.factor(region),
    stp = as.factor(stp),
    msoa = as.factor(msoa),
    rural_urban = as.factor(na_if(rural_urban, -1)),
    imd = na_if(na_if(imd, -1),0), 
    age = na_if(age, -1),
    sex  = as.factor(sex),
    ethnicity = as.factor(na_if(ethnicity, -1)),
    dementia = replace_na(dementia,0),
    
    # Define new variables
    age_ge65 = (age >= 65),   # Identify carehome residents aged >= 65
    ch_ge65 = (care_home_type != "U" & age >= 65),
    institution = (care_home_type == "U" & household_size > 20),    # Identify potential prisons/institutions - still needed?
    household_size_tot = na_if(household_size/(percent_tpp/100), Inf),    # Estimate total household size according to tpp percentage
    test_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss),    # Define delays
    prob_death_delay = as.integer(ons_covid_death_date - primary_care_case_probable),
    HHID = paste(msoa, household_id, sep = ":")    # Redefine unique household identifier
    
    ) %>%    
  as_tibble() 

events <- !is.na(input_clean[,event_dates])
input_clean$case <- (rowSums(events) > 0)
  
print("Summary: Cleaned")
summary(input_clean)

################################################################################

# Save cleaned input data
saveRDS(input_clean, "./input_clean.rds")
write_csv(input_clean, "./input_clean.csv")

# ---------------------------------------------------------------------------- #

sink()

################################################################################
################################################################################

