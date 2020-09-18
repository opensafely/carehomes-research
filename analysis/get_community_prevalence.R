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

# sink("./log_comm_prev.txt")

################################################################################

pacman::p_load("tidyverse", "lubridate", "data.table", "dtplyr")

write("Running get_community_prevalence...",file="data_setup_log.txt", append = TRUE)

# ---------------------------------------------------------------------------- #
#----------------------------#
#   Derive MSOA populations  #
#----------------------------#

input %>%
  group_by(msoa) %>%
  summarise(msoa_pop_total = sum(household_size, na.rm = T),
            msoa_n_practices = n_distinct(practice_id, na.rm = T),
            msoa_pop_ch = sum(household_size[care_home_type != "U"], na.rm = T),
            msoa_pop_comm = sum(household_size[care_home_type == "U"], na.rm = T),
            n_ch = n_distinct(household_id[care_home_type != "U"]), na.rm = T) -> msoa_pop

#----------------------------#
#  Create community dataset  #
#----------------------------#

# Want a dataset of daily incidence of probable cases in the community, with
# estimates of CH/non-CH population per MSOA
  
input %>%
  # split out non-carehome residents who had probable diagnosis 
  filter(care_home_type == "U" & !is.na(primary_care_case_probable)) %>%
  rename(date = primary_care_case_probable) %>%
  # exclude any cases pre-2020 
  filter(date > ymd("2020-01-01")) %>%
  # count probable diagnoses per day and per msoa
  group_by(msoa, date) %>%
  summarise(probable_cases = n()) %>%
  ungroup() -> comm_probable

# define total period of observed events
# (i.e. period for which community prevalence is non-zero)
obs_per <- seq(min(pull(comm_probable,date), na.rm = T),
               max(pull(comm_probable,date), na.rm = T),
               by = "days")

write(paste0("Period of observed probable community cases: ", obs_per[1], "-", obs_per[length(obs_per)]), file="data_setup_log.txt", append = TRUE)

write("Completing rows for each MSOA...", file="data_setup_log.txt", append = TRUE)

# Expand rows in data.table for speed:
start <- Sys.time()
comm_probable <- as.data.table(comm_probable)

# Replicate per region (by vars are all values I want to copy down per date):
all_dates <- comm_probable[,.(date=obs_per),by = "msoa"]

# Merge and fill count with 0:
setkey(comm_probable, msoa, date)
setkey(all_dates, msoa, date)
comm_probable_expand <- comm_probable[all_dates,roll=FALSE]
comm_probable_expand <- comm_probable_expand[is.na(probable_cases), probable_cases:=0]

time <- Sys.time() - start
write(paste0("Finished expanding community dates (time = ",round(time,2),")"), file="data_setup_log.txt", append = TRUE)

comm_probable_expand %>%
  lazy_dt() %>%
  full_join(msoa_pop) %>%
  mutate(probable_cases_rate = probable_cases*1e6/msoa_pop_comm) %>%
  group_by(msoa) %>%
  mutate(probable_cases_rate_total = sum(probable_cases)/unique(msoa_pop_comm)) %>%
  ungroup() %>%
  as.data.frame() -> comm_prev

#----------------------------#
#        Save output         #
#----------------------------#

write.csv(comm_prev, "./data/community_prevalence.csv", row.names = FALSE)
saveRDS(comm_prev, "./data/community_prevalence.rds")

################################################################################

# sink()

################################################################################
