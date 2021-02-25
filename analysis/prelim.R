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

sink("./prelim_check_log.txt", type = "output")

options(datatable.old.fread.datetime.character=TRUE)

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args <- c("input.csv","tpp_msoa_coverage.rds", 2)
args = commandArgs(trailingOnly=TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # check for mixed HH/perc TPP agreement
  mutate(perc_tpp_lt100 = (percent_tpp < 100))

## Load MSOA TPP coverage
tpp_cov <- readRDS(args[2])

## MSOA TPP coverage cut off
msoa_cov_cutoff <- args[3]

# ---------------------------------------------------------------------------- #

# First filter to England
input <- input_raw %>%
  filter(grepl("E",msoa))

# ---------------------------------------------------------------------------- #

#----------------------#
#  PRELIMINARY CHECKS  #
#----------------------#

print("No. unique MSOAs with patients registered in TPP:")
n_distinct(input$msoa)

print("Patients with missing MSOA:")
sum(is.na(input$msoa))

print("Patients with missing household_id: N = ")
nrow(input[input$household_id <= 0,])

input <- input %>%
  filter(!is.na(msoa) & household_id > 0)

print("No. unique MSOAs after filter missing MSOA/ID:")
n_distinct(input$msoa)

# Check consistency of mixed_household and percent_TPP vars
input %>%
  group_by(mixed_household, perc_tpp_lt100) %>%
  tally()

# Check match with population estimates
input %>%
  # Join with MSOA coverage data
  anti_join(tpp_cov, by = "msoa") -> nonmatch_msoa

unique(nonmatch_msoa$msoa)

nrow(nonmatch_msoa)
summary(nonmatch_msoa) 

# ---------------------------------------------------------------------------- #

#--------------------------#
#  Exclude low covg MSOAs  #
#--------------------------#

input <- input %>%
  # Join with MSOA coverage data (only rows with a match)
  inner_join(tpp_cov, by = "msoa")
  
# Filter MSOAs by TPP coverage
exclude_msoa <- input %>%
  filter(tpp_cov < msoa_cov_cutoff) %>%
  pull(msoa) %>%
  unique()

print(paste0("MSOAs excluded with ",msoa_cov_cutoff,"% coverage cut off: n = ",length(exclude_msoa)))

input <- filter(input, !msoa %in% exclude_msoa)

# ---------------------------------------------------------------------------- #

#----------------------#
#    CHECK COUNTS      #
#----------------------#

print("Total Patients")
n_distinct(input$patient_id)

print("Patients with missing HH MSOA:")
sum(is.na(input$msoa))

print("Patients with missing HH type:")
sum(is.na(input$care_home_type))

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

print("HHs with missing size: n = ")
input %>%
  dplyr::select(household_id, household_size) %>%
  group_by(household_id) %>%
  # Select largest size among all residents
  slice_max(household_size, n = 1) %>%
  # HHs for which no resident has non-zero HH size
  filter(household_size <= 0) %>%
  pull(household_id) %>%
  n_distinct()

print("No. households + patients per carehome type:")
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id)) 

print("Probable prisons/institutions (size>15 and not CH)")
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id)) 

print("Care homes registered under > 1 system:")
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id)) 

print("Care homes with < 100% coverage:")
input %>%
  filter(care_home_type != "U") %>%
  group_by(percent_tpp < 100) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n_distinct(patient_id)) 

print("Care homes % TPP coverage:")
summary(
  input %>%
    filter(care_home_type != "U") %>%
    dplyr::select(msoa, household_id, percent_tpp) %>%
    unique() %>% 
    pull(percent_tpp)
)


# ---------------------------------------------------------------------------- #

sink()
