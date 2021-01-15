################################################################################
# Description: Script to set up analysis data for "Spatiotemporal risk of
# infection of care homes during the first wave of the COVID-19 pandemic in the
# UK"
#
# input: individual patient GP record data extracted from OpenSAFELY according
# to "./analyis/study_definition.py".
# output: aggregated dataset for landmark analysis of 14-day care home
# infection risk.
#
# Author: Emily S Nightingale
# Date: 06/08/2020
# 
#
################################################################################

time_total <- Sys.time()

################################################################################

library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(zoo)


options(datatable.old.fread.datetime.character=TRUE)

# Function to calculate mode value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Set study period (excluding last month for testing - too few new CH introductions at this point of epidemic?)
study_per <- seq(as.Date("2020-04-15"),as.Date("2020-09-30"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, postitive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")
dates <- c(event_dates,"discharge_date")

# Time horizon for prediction
ahead <- 14

# write("Data setup log",file="data_setup_log.txt")
sink("data_setup_log.txt", type = "output")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args <- c("./output/input.csv","tpp_msoa_coverage.rds", 90)
args = commandArgs(trailingOnly=TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") 
tpp_cov <- readRDS(args[2])
ch_cov_cutoff <- args[3]

# ---------------------------------------------------------------------------- #
#----------------------#
#  TIDY DATA   #
#----------------------#

# drop rows with missing msoa or carehome flag, set up variable formats and join
# msoa populations

replace_old_dates <- function(x) {
  x[x < '2020-01-01'] <- NA
  return(as_date(x))
}

print("Summary: Raw input")
summary(input_raw)

input <- input_raw %>%
  # Filter just to records from England
  filter(grepl("E",msoa)) %>%
  # drop missing MSOA and care home type
  filter(!is.na(msoa) & !is.na(care_home_type)) %>%
  # exclude potential prisons/institutions
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  # set up var formats
  mutate(dementia = replace_na(dementia,0),
         ethnicity = as.factor(ethnicity),
         rural_urban = as.factor(rural_urban),
         # redefine -1 values as na
         across(c(age, ethnicity, imd, rural_urban), function(x) na_if(x,-1)),
         # replace any dates < 2020 as na
         across(all_of(dates), ymd),
         across(all_of(dates), replace_old_dates)
         ) %>% 
  left_join(tpp_cov, by = "msoa") %>%
  mutate(across(where(is.character), as.factor))

# mutate(dat, dist = ifelse(speed == 4, dist * 100, dist)

print("Summary: Cleaned input")
summary(input)

# Run script to aggregate non-carehome cases by MSOA
source("./analysis/get_community_prevalence.R")

# Split out carehome residents
input %>%
  filter(care_home_type != "U") -> ch

# ---------------------------------------------------------------------------- #

# Remove care homes with low TPP coverage
ch %>%
  filter(percent_tpp > ch_cov_cutoff) -> ch

#-----------------------------#
#  Care home characteristics  #
#-----------------------------#

# Summarise care home resident characteristics
# **will be replaced with CQC vars when codelists available**
# NOTE: multiple events in same HH may have different sizes in dummy data
#       same HH mas have both rural/urban and multiple IMD values in dummy data

ch_chars <- ch %>%
  group_by(household_id, msoa) %>%
  summarise(n_resid = n(),                        # number of individuals registered under CHID
            ch_size = median(household_size),     # TPP-derived household size - discrepancies with n_resid and CQC number of beds?
            ch_type = unique(care_home_type)[1],  # Care, nursing, other
            rural_urban = unique(rural_urban)[1], # Rural/urban location classification 
            imd = unique(imd)[1],                 # Average IMD of MSOA/specific CH location? 
            hh_med_age = median(age, na.rm = T),  # average age of registered residents
            hh_p_female = mean(sex == "F"),       # % registered residents female
            hh_maj_ethn = getmode(ethnicity),     # majority ethnicity of registered residents (5 categories)
            hh_p_dem = mean(dementia)) %>%        # % registered residents with dementia - implies whether care home is dementia-specific
  ungroup() 

print("Summary: Care home characteristics")
summary(ch_chars)

#-----------------------------#
#    Care home first event    #
#-----------------------------#

# Identify first covid event in care home, out of all possible events of interest.
# Care homes which don't have an event in period are assigned the date "3000-01-01"

ch_first_event <- ch %>%
  mutate_at(vars(all_of(event_dates)), function(x) replace_na(ymd(x),ymd("3000-01-01"))) %>%
  group_by(household_id, msoa) %>%
  summarise_at(vars(all_of(event_dates)),min) %>%
  ungroup() %>%
  rename_at(-1:-2, function(x) paste0("first_",x)) %>% 
  rowwise() %>%
  mutate(first_event = ymd(replace_na(min(c_across(starts_with("first_"))),"3000-01-01")),
         ever_affected = (first_event < ymd("3000-01-01")),
         first_event_which = as.factor(event_dates[which.min(c_across(starts_with("first_")))]))
ch_first_event$first_event_which[!ch_first_event$ever_affected] <- NA

print("Summary: First care home events")
summary(ch_first_event)

# Join care home characteristics with first event dates 
ch_wevent <- ch_chars %>%
  full_join(ch_first_event) %>%
  mutate(date = first_event) %>%
  select(-first_primary_care_case_probable:-first_ons_covid_death_date) 

# Expand rows in data.table for speed:
start <- Sys.time()
vars <- names(select(ch_wevent, household_id:hh_p_dem,first_event:first_event_which))
ch_wevent <- as.data.table(ch_wevent)

# Replicate per region (by vars are all values I want to copy down per date):
all_dates <- ch_wevent[,.(date=study_per),by = vars]

# Merge and fill count with 0:
setkey(ch_wevent, household_id, msoa, n_resid, ch_size, ch_type, rural_urban, imd, hh_med_age, hh_p_female, hh_maj_ethn, hh_p_dem, first_event, ever_affected, first_event_which, date)
setkey(all_dates, household_id, msoa, n_resid, ch_size, ch_type, rural_urban, imd, hh_med_age, hh_p_female, hh_maj_ethn, hh_p_dem, first_event, ever_affected, first_event_which, date)
ch_wevent <- ch_wevent[all_dates,roll=TRUE]
# ch_wevent <- ch_wevent[is.na(probable_cases), probable_cases:=0]

# Finished expanding carehome dates: time = 
round(Sys.time() - start,2)

# write(paste0("Finished expanding carehome dates (time = ",
#              round(time,2),
#              ")"), 
#       file="data_setup_log.txt", append = TRUE)

#-----------------------------#
#   Discharges to care home   #
#-----------------------------#

# Number of hospital discharges back to care home per day (assuming resident is
# discharged back to home)
ch %>%
  filter(!is.na(discharge_date)) %>%
  group_by(discharge_date, household_id, msoa) %>%
  count(name = "n_disch") %>%
  ungroup() %>%
  rename(date = discharge_date) -> disch

# Join with discharges: keep only those which occurred within study period
ch_wdisch <- ch_wevent %>%
  lazy_dt() %>%
  group_by_at(vars(household_id:hh_p_dem,first_event, ever_affected)) %>%
  left_join(disch) %>%
  mutate(n_disch = replace_na(n_disch, 0)) %>%
  as.data.frame()

#-----------------------------#
#       Analysis dataset      #
#-----------------------------#

# Join with community prevalence data and define
# 7-day rolling mean/difference + lags.
# For each date, define event_ahead = 1 if that care home's first event occurs
# in next <ahead> days

ch_long <- comm_prev %>%
  right_join(ch_wdisch) %>% #View()
  group_by(household_id) %>%
  mutate(day = 1:n(),
         disch_sum7 = rollsum(n_disch, 7, fill = NA, align = "right"),
         probable_roll7 = rollmean(probable_cases_rate, 7, fill = NA, align = "right"),
         probable_chg7 = probable_cases_rate - lag(probable_cases_rate, 7),
         probable_roll7_lag1wk = lag(probable_roll7, 7),
         probable_roll7_lag2wk = lag(probable_roll7, 14),
         event_ahead = replace_na(as.numeric(
           first_event %within% interval(date,date+ahead)
           ),0)) %>%
  ungroup()


# ---------------------------------------------------------------------------- #

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

print("Summary: Analysis data")
summary(dat)

dat %>%
  group_by(day, event_ahead) %>%
  count() %>%
  head(20)

# ---------------------------------------------------------------------------- #
# Save analysis data

saveRDS(input, file = "./input_clean.rds")
saveRDS(ch, file = "./ch_linelist.rds")
saveRDS(ch_long, file = "./ch_agg_long.rds")
saveRDS(dat, file = "./analysisdata.rds")

################################################################################

# Total time running data_setup:
round(Sys.time() - time_total,2)

# write(paste0("Total time running data_setup: ",
#              round(time_total,2)), 
#       file="data_setup_log.txt", append = TRUE)

sink()

################################################################################

