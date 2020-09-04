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
################################################################################

#---------#
#  SETUP  #
#---------#

pacman::p_load("tidyverse", "lubridate", "data.table", "dtplyr", "zoo", "sandwich", "boot", "lmtest")

# Function to calculate mode value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Read data
args = commandArgs(trailingOnly=TRUE)
input <- fread(args[1], data.table = FALSE) %>%
  mutate_if(is.character, as.factor)

# Split carehome and community residents
ch <- filter(input, care_home_type != "U")
comm <- filter(input, care_home_type == "U")

# Set study period (excluding last month for testing - too few new CH introductions at this point of epidmeic?)
study_per <- seq(as.Date("2020-04-15"),as.Date("2020-06-30"), by = "days")

# Identify vars containing event dates (1-3 combined describe "probable" cases)
event_dates <- c("primary_care_case_clinical","primary_care_case_test", "primary_care_case_seq","first_pos_test_sgss","ons_covid_death_date")

# Time horizon for prediction
ahead <- 14

# ---------------------------------------------------------------------------- #

#------------------------#
#  Community prevalence  #
#------------------------#

# Aggregate probable case records from non-carehome population, per MSOA.
#  - Need to scale up for % TPP coverage in MSOA?
#  - Double counting if include primary_care_case_test + first_pos_test_sgss?

comm_events <- comm %>%
  pivot_longer(all_of(event_dates[1:3]),
               values_to = "date",
               names_to = "event_type") %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(date))

# define total period of observed events
# (i.e. period for which community prevalence available)
obs_per <- seq(min(comm_events$date, na.rm = T),
               max(comm_events$date, na.rm = T),
               by = "days")

# expand rows for each MSOA between earliest and latest observed event
comm_events %>%
  complete(date = obs_per,
           nesting(msoa, event_type),
           fill = list(event = 0)) %>%
  group_by(msoa,date,event_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "event_type",values_from = "n") %>%
  mutate(probable_cases =
           primary_care_case_clinical +
           primary_care_case_test +
           primary_care_case_seq) -> comm_events_agg


#------------------#
#  Care home data  #
#------------------#

# Descriptive
# Summarise care home resident characteristics
# **will be replaced with CQC vars when codelists available**
# NOTE: multiple events in same HH have different sizes in dummy data

ch_chars <- ch %>%
  filter(care_home_type != "U") %>%
  mutate(dementia = replace_na(dementia,0),
         msoa = as.factor(msoa)) %>%
  group_by(household_id, msoa) %>%
  summarise(n_patients = n(),
            ch_size = median(household_size),
            hh_avg_age = mean(age, na.rm = T),
            hh_p_female = mean(sex == "F"),
            hh_maj_ethn = getmode(ethnicity),
            hh_p_dem = mean(dementia)) %>%
  ungroup()

summary(ch_chars)

# First event
# Identify first covid event in care home, out of all possible events of interest.
# Care homes which don't have an event in period are assigned the date "3000-01-01"

ch_first_event <- ch %>%
  filter(care_home_type != "U") %>%
  mutate_at(vars(all_of(event_dates)), function(x) replace_na(ymd(x),ymd("3000-01-01"))) %>%
  group_by(household_id, msoa) %>%
  summarise_at(vars(all_of(event_dates)),min) %>%
  ungroup() %>%
  rename_at(-1:-2, function(x) paste0("first_",x)) %>%
  rowwise() %>%
  mutate(first_event = ymd(min(c_across(starts_with("first_"))))) %>% #gsub("3000-01-01",NA,
  filter(!is.na(first_event))

summary(ch_first_event)

#Expand rows for whole time period then join with community data:
# Join with care home characteristics and define earliest event across all
# probable case definitions (clinical/test/sequelae/death)

ch_wevent <- ch_chars %>%
  full_join(ch_first_event) %>%
  mutate(date = first_event) %>%
  # home characteristics to copy down:
  group_by_at(vars(household_id:hh_p_dem,first_event)) %>%
  # expand rows for each home for every date in defined study period:
  complete(date = study_per) %>%
  ungroup() %>%
  dplyr::select(household_id:hh_p_dem, date, first_event)

# Join with community prevalence variables, sum all probable cases and define
# 7-day rolling mean/difference.
# For each date, define event_ahead = 1 if that care home's first event occurs
# in next <ahead> days

ch_long <- comm_events_agg %>%
  right_join(ch_wevent) %>% #View()
  group_by(household_id) %>%
  mutate(day = 1:n(),
         comm_probable_roll7 = rollmean(probable_cases, 7, fill = NA),
         comm_probable_chg7 = probable_cases - lag(probable_cases, 7),
         comm_probable_lag7 = lag(probable_cases, 7),
         comm_probable_lag6 = lag(probable_cases, 6),
         comm_probable_lag5 = lag(probable_cases, 5),
         comm_probable_lag4 = lag(probable_cases, 4),
         comm_probable_lag3 = lag(probable_cases, 3),
         comm_probable_lag2 = lag(probable_cases, 2),
         comm_probable_lag1 = lag(probable_cases, 1),
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
lmk_data <- bind_rows(lapply(1:length(study_per), make_data_t))

# View(lmk_data)
# str(lmk_data)
# summary(lmk_data)
# lazy_dt(lmk_data) %>%
#   group_by(day, event_ahead) %>%
#   count() %>%
#   as.data.frame()

# ---------------------------------------------------------------------------- #
# Save analysis data

save(lmk_data, file = "./output/analysisdata.rds")
