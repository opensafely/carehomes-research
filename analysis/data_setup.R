################################################################################
# Description: Set up analysis data of care home daily survival, including care 
# home characteristics, covid introduction events, hospital discharges and 
# community prevalence of probable infections.
#
# input: Cleaned input data from data_clean.R
# output: Aggregated dataset for landmark analysis of 14-day care home
# infection risk.
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################
################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(zoo)

# write("Data setup log",file="data_setup_log.txt")
sink("data_setup_log.txt")

# Function: calculate mode value across residents in household
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Household characteristics were defined according to address on 01/02/2020.
# household_id will be updated when patient relocates but characteristics remain fixed.
# Therefore households may have a mix of values for each characteristics from 
# residents who moved since Feb 2020. 

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args <- c("./input_clean.rds","./data/cases_rolling_nation.csv", 50)
args = commandArgs(trailingOnly = TRUE)

input <- readRDS(args[1]) 
case_eng <- read.csv(args[2])
ch_cov_cutoff <- as.numeric(args[3])

# Identify vars containing event dates: probable covid identified via primary care, positive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")

# Time horizon for prediction
ahead <- 14

# ---------------------------------------------------------------------------- #

# Run script to aggregate non-carehome cases by MSOA
source("./analysis/get_community_incidence.R")

print("Summary: Daily community incidence")
summary(comm_inc)

# Set study period based on period available in community incidence
study_per <- seq(min(comm_inc$date), as.Date("2020-12-07"), by = "days")

# ---------------------------------------------------------------------------- #

# Check number of individuals in community and aged 65+ in care homes
input %>%
  group_by(care_home_type) %>%
  tally()

input %>%
  group_by(care_home_type, ch_ge65) %>%
  tally()

# Split out carehome residents 
input %>%
  filter(ch_res) -> ch

print("Summary: all care home residents")
summary(ch)

# ---------------------------------------------------------------------------- #

#-----------------------------#
#  Care home characteristics  #
#-----------------------------#

# Summarise care home resident characteristics
# **will be replaced with CQC vars when codelists available**
# NOTE: multiple events in same HH may have different sizes in dummy data
#       same HH may have both rural/urban and multiple IMD values in dummy data

ch_chars <- ch %>%
  group_by(household_id) %>%
  summarise(percent_tpp = getmode(percent_tpp),
            mixed_household = getmode(mixed_household),
            exclude = (is.na(percent_tpp) | percent_tpp < ch_cov_cutoff),
            region = getmode(region),
            msoa = getmode(msoa),
            n_resid = n(),                        # number of individuals registered under CHID
            ch_size = unique(household_size_tot),     # TPP-derived household size - discrepancies with n_resid and CQC number of beds?
            ch_type = getmode(care_home_type),    # Care, nursing, other
            rural_urban8 = getmode(rural_urban),  # Rural/urban location classification - select mode value over all residents
            rural_urban8_miss = sum(is.na(rural_urban)), 
            imd = getmode(imd),                   # In case missing for some indivs, take mode over HH residents
            imd_miss = sum(is.na(rural_urban)), 
            hh_med_age = median(age),             # average age of registered residents
            age_miss = sum(is.na(age)), 
            hh_p_female = mean(sex == "F"),       # % registered residents female
            # hh_maj_ethn = getmode(ethnicity),     # majority ethnicity of registered residents (5 categories)
            # ethn_miss = sum(is.na(ethnicity)), 
            hh_p_min = mean(ethnicity != 1, na.rm = T),
            hh_p_dem = mean(dementia, na.rm = T),
            n_case = sum(case)) %>%        # % registered residents with dementia - implies whether care home is dementia-specific
  ungroup() %>%
  mutate(imd_quint = as.factor(cut(imd, 5)),
         hh_maj_dem = (hh_p_dem >= 0.5),
         rural_urban = as.factor(case_when(rural_urban8 %in% 1:4 ~ "urban",
                                           rural_urban8 %in% 5:8 ~ "rural")))

print("Summary: All care home characteristics")
summary(ch_chars)

print(paste0("No. unique homes in selected MSOAs:", nrow(ch_chars)))

# ---------------------------------------------------------------------------- #
# Exclude care homes on TPP coverage

# For homes in which some residents lived elsewhere in Feb 2020, the MSOA listed
# may not be unique, hence the coverage won't be unique. Therefore we exclude based
# on the coverage of the MSOA listed for the majority of residents within that
# household.

print(paste0("No. homes with residents under >1 system: ",
             sum(ch_chars$mixed_household == 1)))

print("% TPP coverage - by household:")
summary(ch_chars$percent_tpp)
summary(
  ch_chars %>%
    mutate(percent_tpp_cat = cut(percent_tpp, 
                                 breaks = 10,
                                 include.lowest = TRUE)) %>%
    pull(percent_tpp_cat)
)

ch_bycovg <- split(ch_chars, ch_chars$exclude) %>%
  lapply(FUN = function(x) unique(pull(x, household_id)))

excl <- ch_bycovg[["TRUE"]]
incl <- ch_bycovg[["FALSE"]]

print(paste0("Care homes excluded with ",ch_cov_cutoff,"% coverage cut off: n = ",length(excl)))
print(paste0("Care homes included with ",ch_cov_cutoff,"% coverage cut off: n = ",length(incl)))

# Keep only homes with sufficient coverage
ch_chars <- ch_chars %>%
  filter(household_id %in% incl)

print("Summary: Included care home characteristics")
summary(ch_chars)

# Also keep only residents in homes with sufficient coverage
ch <- ch %>%
  filter(household_id %in% incl)

# ---------------------------------------------------------------------------- #
# Check uniqueness of characteristics per household

print("Uniqueness of household characteristics over residents (included homes):")
ch %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa, na.rm = T), 
            region = n_distinct(region, na.rm = T),
            household_size_tot = n_distinct(household_size_tot, na.rm = T),
            care_home_type = n_distinct(care_home_type, na.rm = T),
            imd = n_distinct(imd, na.rm = T),
            rural_urban = n_distinct(rural_urban, na.rm = T)) %>%
  ungroup() -> n_distinct_chars
# Should be one distinct value for every household
summary(n_distinct_chars)

print("No. included care homes with non-unique characteristics across residents:")
n_distinct_chars %>%
  dplyr::select(-household_id) %>%
  summarise(across(everything(), function(x) sum(x > 1)))

# ---------------------------------------------------------------------------- #

#-----------------------------#
#    Care home first event    #
#-----------------------------#

# Identify first covid event in care home, out of all possible events of interest.
# Exclude care homes with first event prior to 2020-04-15
# Care homes which don't have any event in period are assigned the date "3000-01-01"

ch_first_event <- ch %>%
  mutate_at(vars(all_of(event_dates)), function(x) replace_na(ymd(x),ymd("3000-01-01"))) %>%
  group_by(household_id) %>%
  summarise_at(vars(all_of(event_dates)),min) %>%
  ungroup() %>%
  rename_at(-1, function(x) paste0("first_",x)) %>% 
  rowwise() %>%
  mutate(first_event = ymd(replace_na(min(c_across(starts_with("first_"))),"3000-01-01")),
         first_event_which = as.factor(event_dates[which.min(c_across(starts_with("first_")))])) %>%
  group_by(household_id) %>%
  mutate(first_event_pre_per = (first_event < min(study_per)),
         first_event_post_per = (first_event > max(study_per) & first_event < ymd("3000-01-01")),
         ever_affected = between(first_event, min(study_per), max(study_per))) 
ch_first_event$first_event_which[!ch_first_event$ever_affected] <- NA

print("Care homes with first event prior to study period (excluded from analysis):")
ch_first_event %>%
  group_by(first_event_pre_per) %>%
  tally()

print("Care homes with first event posterior to study period (excluded from analysis):")
ch_first_event %>%
  group_by(first_event_post_per) %>%
  tally()

# Join care home characteristics with first event dates 
ch_wevent <- ch_chars %>%
  full_join(ch_first_event) %>%
  # Exclude care homes with first event prior to study period
  filter(!first_event_pre_per) %>%
  mutate(date = first_event) %>%
  select(-first_primary_care_case_probable:-first_ons_covid_death_date,
         -rural_urban8, -rural_urban8_miss, -imd_miss, -age_miss,
         -first_event_pre_per, -first_event_post_per) 

print("Summary: First events")
summary(ch_wevent)

print("Care homes affected during study period:")
ch_wevent %>%
  group_by(ever_affected) %>%
  tally()

print("Summary: Characteristics of care homes affected during study period")
ch_wevent %>%
  filter(ever_affected) %>%
  summary()

# ---------------------------------------------------------------------------- #

#-----------------------------#
#         Expand rows         #
#-----------------------------#

# Expand rows in data.table for speed:
vars <- names(select(ch_wevent, -date))
ch_wevent <- as.data.table(ch_wevent)

# Replicate per region (by vars are all values I want to copy down per date):
all_dates <- ch_wevent[,.(date = study_per),by = vars]

# Merge and fill count with 0:
setkey(ch_wevent, 
       exclude, household_id, percent_tpp, region, msoa, n_resid, ch_size, ch_type, 
       rural_urban, imd, imd_quint, 
       hh_med_age, hh_p_female, hh_p_min, hh_p_dem, hh_maj_dem, 
       n_case, first_event, first_event_which, ever_affected, 
       date)
setkey(all_dates, 
       exclude, household_id, percent_tpp, region, msoa, n_resid, ch_size, ch_type, 
       rural_urban, imd, imd_quint, 
       hh_med_age, hh_p_female, hh_p_min, hh_p_dem, hh_maj_dem, 
       n_case, first_event, first_event_which, ever_affected, 
       date)

ch_wevent <- ch_wevent[all_dates,roll = TRUE]

# ---------------------------------------------------------------------------- #

#-----------------------------#
#    Setup analysis dataset   #
#-----------------------------#

# Join with community prevalence data and define
# 7-day rolling mean/difference + lags.
# For each date, define event_ahead = 1 if that care home's first event occurs
# in next <ahead> days

ch_long <- comm_inc %>%
  right_join(ch_wevent) %>% #View()
  group_by(household_id) %>%
  mutate(day = 1:n(),
         wave = factor(date >= ymd("2020-08-01"), labels = c("first","second")),
         event_ahead = replace_na(as.numeric(
           first_event %within% interval(date,date + ahead)),
           0)) %>%
  ungroup()

print("Homes in ch_long data:")
ch_long %>%
  group_by(exclude, ever_affected) %>%
  summarise(N = n_distinct(household_id))

#To create the dataset for landmarking analysis, need to define a subset for
#each day of carehomes which have not yet had an event and then bind all subsets
#together.

make_data_t <- function(t, ahead = 14){
  # filter to day t and drop CHs who had event before t
  data <- filter(ch_long, day == t & !(first_event <= study_per[t]))
  return(data)
}

# apply function for each date in range and bind
dat <- bind_rows(lapply(1:length(study_per), make_data_t))

print("No. homes in full analysis data:")
dat %>%
  group_by(exclude) %>%
  summarise(N = n_distinct(household_id))

# ---------------------------------------------------------------------------- #
# Save analysis data

saveRDS(comm_inc, "./community_incidence.rds")
saveRDS(ch, file = "./ch_linelist.rds")
saveRDS(ch_long, file = "./ch_agg_long.rds")
saveRDS(dat, file = "./analysisdata.rds")

# ---------------------------------------------------------------------------- #

sink()

################################################################################

