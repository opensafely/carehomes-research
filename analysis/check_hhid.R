################################################################################
# Description: Check uniqueness of household ID
#
################################################################################

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(data.table)

sink("./check_hhid.txt", type = "output")

options(datatable.old.fread.datetime.character = TRUE)

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# args <- c("input.csv")
args = commandArgs(trailingOnly = TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # Filter just to records from England
  filter(grepl("E",msoa)) %>%
  mutate(HHID = paste(msoa, household_id, sep = ":"),
         across(c(imd, rural_urban), function(x) na_if(x,-1)),
         across(c(imd, household_size, household_id), function(x) na_if(x,0)))

#----------------------------------------#
#      UNIQUENESS OF HOUSEHOLD ID        #
#----------------------------------------#

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

# Should be one distinct value for every household
summary(n_distinct_chars)

# ---------------------------------------------------------------------------- #

#----------------------------------#
#      CHECK HOUSEHOLD SIZES       #
#----------------------------------#

print("Household size by care home type:")
input %>%
  filter(!is.na(household_size)) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_size),
            sd = sd(household_size),
            median = median(household_size),
            minmax = paste(min(household_size), max(household_size), sep = ", ")) 

print("Number of records by care home type (household_id):")
input %>%
  group_by(care_home_type, household_id) %>%
  summarise(n_resid = n()) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(n_resid),
            sd = sd(n_resid),
            median = median(n_resid),
            minmax = paste(min(n_resid), max(n_resid), sep = ", ")) 

print("Number of records by care home type (HHID):")
input %>%
  group_by(care_home_type, HHID) %>%
  summarise(n_resid = n()) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(n_resid),
            sd = sd(n_resid),
            median = median(n_resid),
            minmax = paste(min(n_resid), max(n_resid), sep = ", ")) 

################################################################################

sink()

################################################################################
