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
# NOTES: 
# * Community risk so far only calculated from TPP so will be underestimated in 
#   low-TPP coverage MSOAs. 
# * 
#
################################################################################

sink("./log_data_setup.txt")

################################################################################


pacman::p_load("tidyverse", "lubridate", "data.table", "dtplyr", "zoo", "sandwich", "boot", "lmtest")

# Function to calculate mode value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Set study period (excluding last month for testing - too few new CH introductions at this point of epidmeic?)
study_per <- seq(as.Date("2020-04-15"),as.Date("2020-06-30"), by = "days")

# Identify vars containing event dates: probable covid identified via primary care, postitive test result, covid-related hospital admission and covid-related death (underlying and mentioned)
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date","ons_covid_death_date")

# Time horizon for prediction
ahead <- 14

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD AND TIDY DATA  #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates
#   - *NOT YET* pooled between TPP/EMIS

args <- c("./output/input.csv")
# args = commandArgs(trailingOnly=TRUE)

input_raw <- fread(args[1], data.table = FALSE, na.strings = "") 

input <- input_raw %>%
  # drop missing household ID, practice ID, care home type
  filter(!is.na(household_id) & !is.na(practice_id) & !is.na(care_home_type)) %>%
  # set up var formats
  mutate_if(is.character, as.factor) %>%
  mutate(dementia = replace_na(dementia,0),
         ethnicity = as.factor(ethnicity)) %>%
  mutate_at(all_of(event_dates), ymd)

paste0("HHs with missing ID/GP/type: n = ", n_distinct(input_raw$household_id) - n_distinct(input$household_id))
paste0("Patients with missing HH ID/GP/type: n = ", n_distinct(input_raw$household_id) - n_distinct(input$household_id))

summary(input)

# Split out carehome residents
ch <- filter(input, care_home_type != "U")

# Load community prevalence data 
comm_prev <- fread("./data/community_prevalence.csv", data.table = FALSE) 

# ---------------------------------------------------------------------------- #

# Remove care homes registered with more than one GP
ch %>%
  group_by(household_id) %>%
  filter(n_distinct(practice_id) == 1) %>%
  ungroup() -> ch_1gp

paste0("Care homes registered with > 1 GP: n = ", n_distinct(ch$household_id) - n_distinct(ch_1gp$household_id))
paste0("Residents of care homes registered with > 1 GP: n = ", nrow(setdiff(ch, ch_1gp)))

ch <- ch_1gp


#-----------------------------#
#  Care home characteristics  #
#-----------------------------#

# Summarise care home resident characteristics
# **will be replaced with CQC vars when codelists available**
# NOTE: multiple events in same HH may have different sizes in dummy data

summary(ch)

ch_chars <- ch %>%
  group_by(household_id, msoa) %>%
  summarise(n_resid = n(),                      # number of individuals registered under CHID
            ch_size = median(household_size),   # TPP-derived household size - discrepancies with n_resid and CQC number of beds?
            hh_avg_age = mean(age, na.rm = T),  # average age of registered residents
            hh_p_female = mean(sex == "F"),     # % registered residents female
            hh_maj_ethn = getmode(ethnicity),   # majority ethnicity of registered residents (5 categories)
            hh_p_dem = mean(dementia)) %>%      # % registered residents with dementia - implies whether care home is dementia-specific
  ungroup()

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
  mutate(first_event = ymd(min(c_across(starts_with("first_"))))) %>% #gsub("3000-01-01",NA,
  filter(!is.na(first_event))

summary(ch_first_event)


# Join with care home characteristics and define earliest across all
# covid events (clinical/test/sequelae/death)

ch_wevent <- ch_chars %>%
  full_join(ch_first_event) %>%
  mutate(date = first_event) %>%
  # home characteristics to copy down:
  group_by_at(vars(household_id:hh_p_dem,first_event)) %>%
  # expand rows for each home for every date in defined study period:
  complete(date = study_per) %>%
  ungroup() %>%
  dplyr::select(household_id:hh_p_dem, date, first_event)


#-----------------------------#
#       Analysis dataset      #
#-----------------------------#

# Join with community prevalence data and define
# 7-day rolling mean/difference + lags.
# For each date, define event_ahead = 1 if that care home's first event occurs
# in next <ahead> days

ch_long <- comm_prev %>%
  right_join(ch_wevent) %>% #View()
  group_by(household_id) %>%
  mutate(day = 1:n(),
         probable_roll7 = rollmean(probable_cases_rate, 7, fill = NA),
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
lmk_data <- bind_rows(lapply(1:length(study_per), make_data_t))

summary(lmk_data)

lmk_data %>%
  group_by(day, event_ahead) %>%
  count() 


# ---------------------------------------------------------------------------- #

# Split data into training and test, and drop any rows with missing predictor values
samp <- sample(unique(ch$household_id),0.8*n_distinct(ch$household_id))
train <- filter(lmk_data, household_id %in% samp) %>% drop_na()
test <- filter(lmk_data, !household_id %in% samp) %>% drop_na()


# ---------------------------------------------------------------------------- #
# Save analysis data (?)

write.csv(lmk_data, file = "./output/analysisdata.csv")


################################################################################

sink() 

################################################################################

