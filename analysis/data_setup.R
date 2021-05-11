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

time_total <- Sys.time()

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

# Function: alculate mode value
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args <- c("./input_clean.rds","./data/cases_rolling_nation.csv", 90)
args = commandArgs(trailingOnly=TRUE)

input <- readRDS(args[1]) 
case_eng <- read.csv(args[2])
ch_cov_cutoff <- args[3]

# Set study period 
study_per <- seq(as.Date("2020-04-15"),as.Date("2020-12-07"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, postitive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")
# dates <- c(event_dates,"discharge_date")

# Time horizon for prediction
ahead <- 14

# ---------------------------------------------------------------------------- #

# Run script to aggregate non-carehome cases by MSOA
source("./analysis/get_community_incidence.R")

print("Summary: Daily community incidence")
summary(comm_inc)

# ---------------------------------------------------------------------------- #

# Split out carehome residents
input %>%
  filter(care_home_type != "U") -> ch

# ---------------------------------------------------------------------------- #

# Remove care homes with low TPP coverage
print(paste0("Care homes included with ",ch_cov_cutoff,"% cut off:"))
ch %>%
  mutate(include = (percent_tpp > ch_cov_cutoff)) %>%
  group_by(include) %>%
  summarise(n_ch = n_distinct(msoa, household_id))

ch %>%
  filter(percent_tpp > ch_cov_cutoff) -> ch_cutoff

print(paste0("Care homes excluded with ",ch_cov_cutoff,"% coverage cut off: n = ",n_distinct(ch$msoa, ch$household_id)-n_distinct(ch_cutoff$msoa, ch_cutoff$household_id)))

ch <- ch_cutoff

print("Summary: care home residents")
summary(ch)

#-----------------------------#
#  Care home characteristics  #
#-----------------------------#

# Summarise care home resident characteristics
# **will be replaced with CQC vars when codelists available**
# NOTE: multiple events in same HH may have different sizes in dummy data
#       same HH mas have both rural/urban and multiple IMD values in dummy data

ch_chars <- ch %>%
  group_by(household_id, msoa) %>%
  summarise(region = getmode(region),
            n_resid = n(),                        # number of individuals registered under CHID
            ch_size = getmode(household_size),    # TPP-derived household size - discrepancies with n_resid and CQC number of beds?
            ch_type = getmode(care_home_type),    # Care, nursing, other
            rural_urban8 = getmode(rural_urban),  # Rural/urban location classification - select mode value over all residents
            rural_urban8_miss = sum(is.na(rural_urban)), 
            imd = getmode(imd),                   # In case missing for some indivs, take mode over HH residents
            imd_miss = sum(is.na(rural_urban)), 
            hh_med_age = median(age),             # average age of registered residents
            age_miss = sum(is.na(age)), 
            hh_p_female = mean(sex == "F"),       # % registered residents female
            hh_maj_ethn = getmode(ethnicity),     # majority ethnicity of registered residents (5 categories)
            ethn_miss = sum(is.na(ethnicity)), 
            hh_prop_min = mean(ethnicity != 1, na.rm = T),
            hh_p_dem = mean(dementia)) %>%        # % registered residents with dementia - implies whether care home is dementia-specific
  ungroup() %>%
  mutate(imd_quint = as.factor(cut(imd, 5)),
         hh_dem_gt25 = (hh_p_dem > 0.25),
         rural_urban = as.factor(case_when(rural_urban8 %in% 1:4 ~ "urban",
                                 rural_urban8 %in% 5:8 ~ "rural")))

print("Summary: Care home characteristics")
summary(ch_chars)

print("No. unique homes:")
nrow(ch_chars)
n_distinct(ch_chars$msoa,ch_chars$household_id)

#-----------------------------#
#    Care home first event    #
#-----------------------------#

# Identify first covid event in care home, out of all possible events of interest.
# Exclude care homes with first event prior to 2020-04-15
# Care homes which don't have any event in period are assigned the date "3000-01-01"

ch_first_event <- ch %>%
  mutate_at(vars(all_of(event_dates)), function(x) replace_na(ymd(x),ymd("3000-01-01"))) %>%
  group_by(msoa, household_id) %>%
  summarise_at(vars(all_of(event_dates)),min) %>%
  ungroup() %>%
  rename_at(-1:-2, function(x) paste0("first_",x)) %>% 
  rowwise() %>%
  mutate(first_event = ymd(replace_na(min(c_across(starts_with("first_"))),"3000-01-01")),
         first_event_which = as.factor(event_dates[which.min(c_across(starts_with("first_")))])) %>%
  group_by(msoa, household_id) %>%
  mutate(first_event_pre_per = (first_event < ymd("2020-04-15")),
         first_event_post_per = (first_event > ymd("2020-12-07") & first_event < ymd("3000-01-01")),
         ever_affected = between(first_event, ymd("2020-04-15"), ymd("2020-12-07"))) 
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
  select(-first_primary_care_case_probable:-first_ons_covid_death_date) 

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

# Expand rows in data.table for speed:
start <- Sys.time()
vars <- names(select(ch_wevent, household_id:rural_urban,first_event:first_event_which, ever_affected))
ch_wevent <- as.data.table(ch_wevent)

# Replicate per region (by vars are all values I want to copy down per date):
all_dates <- ch_wevent[,.(date=study_per),by = vars]

# Merge and fill count with 0:
setkey(ch_wevent, household_id, msoa, region, n_resid, ch_size, ch_type, rural_urban8, 
       imd, hh_med_age, hh_p_female, hh_maj_ethn, hh_prop_min, hh_p_dem, 
       imd_quint, hh_dem_gt25, rural_urban, first_event, ever_affected, 
       first_event_which, date)
setkey(all_dates, household_id, msoa, region, n_resid, ch_size, ch_type, rural_urban8, 
       imd, hh_med_age, hh_p_female, hh_maj_ethn, hh_prop_min, hh_p_dem, 
       imd_quint, hh_dem_gt25, rural_urban, first_event, ever_affected, 
       first_event_which, date)
ch_wevent <- ch_wevent[all_dates,roll=TRUE]
# ch_wevent <- ch_wevent[is.na(probable_cases), probable_cases:=0]

# Finished expanding carehome dates: time = 
round(Sys.time() - start,2)

#-----------------------------#
#       Analysis dataset      #
#-----------------------------#

# Join with community prevalence data and define
# 7-day rolling mean/difference + lags.
# For each date, define event_ahead = 1 if that care home's first event occurs
# in next <ahead> days

ch_long <- comm_inc %>%
  right_join(ch_wevent, by = c("msoa","date")) %>% #View()
  group_by(household_id) %>%
  mutate(day = 1:n(),
         wave = factor(date >= ymd("2020-08-01"), labels = c("first","second")),
         # msoa_roll7 = rollmean(probable_cases_rate, 7, fill = NA, align = "right"),
         # msoa_lag1wk = lag(msoa_roll7, 7),
         # msoa_lag2wk = lag(msoa_roll7, 14),
         # eng_lag1wk = lag(eng_roll7, 7),
         # eng_lag2wk = lag(eng_roll7, 14),
         event_ahead = replace_na(as.numeric(
           first_event %within% interval(date,date+ahead)
           ),0)) %>%
  ungroup()

print("Homes in ch_long data:")
ch_long %>%
  group_by(ever_affected) %>%
  summarise(N = n_distinct(msoa, household_id))

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

print("Summary: community incidence by occurrence of a care home event:")
dat %>% 
  pivot_longer(c("msoa_probable_rate","msoa_roll7","msoa_lag1wk","msoa_lag2wk","eng_roll7","eng_lag1wk","eng_lag2wk")) %>%
  group_by(event_ahead, name) %>%
  summarise(min = min(value, na.rm = T), max = max(value, na.rm = T), mean = mean(value, na.rm = T), sd = sqrt(var(value, na.rm = T)), med = median(value, na.rm = T))

print("Homes in analysis data:")
n_distinct(dat$msoa, dat$household_id)

print("Homes in ch_wevent but not analysis data:")
ch_wevent %>%
  as_tibble() %>%
  filter(!household_id %in% dat$household_id) %>%
  pull(household_id) %>%
  unique() 

dat %>%
  group_by(day, event_ahead) %>%
  count() %>%
  head(20)

# ---------------------------------------------------------------------------- #
# Save analysis data

saveRDS(comm_inc, "./community_incidence.rds")
saveRDS(ch, file = "./ch_linelist.rds")
saveRDS(ch_long, file = "./ch_agg_long.rds")
saveRDS(dat, file = "./analysisdata.rds")

# ---------------------------------------------------------------------------- #

# Total time running data_setup:
round(Sys.time() - time_total,2)

sink()

################################################################################

