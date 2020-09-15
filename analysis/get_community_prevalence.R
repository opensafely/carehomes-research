################################################################################
# Description: Extract daily incidence of probable COVID-19 infection per MSOA.
# To be run with both TPP/EMIS and pooled.
#
# input: individual patient GP record data extracted from OpenSAFELY according
# to "./analyis/study_definition.py". (TPP)
# output: aggregated dataset with probable community cases per day per MSOA, 
# plus number of records in that system per MSOA (population stratified by 
# CH/community and total).
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
# NOTES:
# * Need to add dynamic naming for output when run on different systems
#
################################################################################

sink("./log_comm_prev.txt")

################################################################################

pacman::p_load("tidyverse", "lubridate", "data.table")

#----------------------#
#  LOAD AND TIDY DATA  #
#----------------------#

setwd("~/COVID-19/carehomes-research")
args <- c("./output/input.csv")
# args = commandArgs(trailingOnly=TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") 

# ---------------------------------------------------------------------------- #

# Identify vars containing event dates: probable covid identified via primary care, postitive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date","ons_covid_death_date")

input <- input_raw %>%
  # drop missing MSOA and care home type
  filter(!is.na(msoa) & !is.na(care_home_type)) %>%
  # set up var formats
  mutate_if(is.character, as.factor) %>%
  mutate_at(all_of(event_dates), ymd)

paste0("HHs with missing MSOA/type: n = ", n_distinct(input_raw$household_id) - n_distinct(input$household_id))
paste0("Patients with missing HH MSOA/type: n = ", n_distinct(input_raw$household_id) - n_distinct(input$household_id))

summary(input)

# Derive MSOA populations
input %>%
  group_by(msoa) %>%
  summarise(msoa_pop_total = n(),
         msoa_pop_ch = sum(care_home_type != "U"),
         msoa_pop_comm = sum(care_home_type == "U"),
         msoa_n_practices = n_distinct(practice_id),
         n_ch = n_distinct()) -> msoa_pop

#----------------------------#
#  Create community dataset  #
#----------------------------#

# Want a dataset of daily incidence of probable cases in the community, with
# estimates of CH/non-CH population per MSOA
  
input %>%
  # split out non-carehome residents who had probable diagnosis 
  filter(care_home_type == "U" & !is.na(primary_care_case_probable)) %>%
  # count probable diagnoses per day and per msoa
  rename(date = primary_care_case_probable) %>%
  group_by(msoa, date) %>%
  summarise(probable_cases = n()) %>%
  ungroup() -> comm_probable

# define total period of observed events
# (i.e. period for which community prevalence is non-zero)
obs_per <- seq(min(comm_probable$date, na.rm = T),
               max(comm_probable$date, na.rm = T),
               by = "days")

# expand rows for each MSOA between earliest and latest observed event and define rate per 100,000 community population
comm_probable %>%
  complete(date = obs_per,
           nesting(msoa), 
           fill = list(probable_cases_comm = 0)) %>% 
  full_join(msoa_pop) %>%
  mutate(probable_cases_rate = probable_cases*1e6/msoa_pop_comm) -> comm_probable_expand


#----------------------------#
#        Save output         #
#----------------------------#

write.csv(comm_probable_expand, "./data/community_prevalence.csv", row.names = FALSE)


################################################################################

sink()

################################################################################
