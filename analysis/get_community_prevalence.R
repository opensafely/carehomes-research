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

write("Running get_community_prevalence...",file="data_setup_log.txt", append = TRUE)

# ---------------------------------------------------------------------------- #

msoa_nb <- read_csv("./data/msoa_neighbours_full.csv")

#----------------------------#
#  Create community dataset  #
#----------------------------#

# Want a dataset of daily incidence of probable cases in the community, with
# estimates of CH/non-CH population per MSOA

# Count number of patients and unique MSOAs in TPP without a carehome flag
input %>%
  filter(care_home_type == "U" & !institution) %>%
  summarise(n = n(), msoa = n_distinct(msoa)) -> comm_tally

print(paste0("N = ",comm_tally$n," non-carehome residents across ",comm_tally$msoa," MSOAs"))

input %>%
  # split out non-carehome residents who had probable diagnosis 
  filter(care_home_type == "U" & !is.na(primary_care_case_probable)) %>%
  rename(date = primary_care_case_probable,
         tpp_cov = tpp_cov_wHHID,
         tpp_pop = tpp_pop_wHHID) %>%
  # exclude any cases pre-2020 
  filter(date > ymd("2020-01-01")) %>%
  # count probable diagnoses per day and per msoa
  group_by(msoa, tpp_pop, msoa_pop, `70+`, tpp_cov, date) %>%
  summarise(probable_cases = n()) %>%
  ungroup() -> comm_probable

# define total period of observed events
# (i.e. period for which community prevalence is non-zero)
obs_per <- seq(min(pull(comm_probable,date), na.rm = T),
               max(pull(comm_probable,date), na.rm = T),
               by = "days")

print(paste0("Period of observed probable community cases: ", obs_per[1], "-", obs_per[length(obs_per)]))

print("Completing rows for each MSOA...")

# Expand rows in data.table for speed:
start <- Sys.time()
comm_probable <- as.data.table(comm_probable)

# Replicate per region (by vars are all values I want to copy down per date):
all_dates <- comm_probable[,.(date=obs_per),by = c("msoa","tpp_pop", "msoa_pop", "70+", "tpp_cov"),]

# Merge and fill count with 0:
setkey(comm_probable, msoa, tpp_pop, msoa_pop, `70+`, tpp_cov, date)
setkey(all_dates, msoa, tpp_pop, msoa_pop, `70+`, tpp_cov, date)
comm_probable_expand <- comm_probable[all_dates,roll=FALSE]
comm_probable_expand <- comm_probable_expand[is.na(probable_cases), probable_cases:=0]

time <- Sys.time() - start
print(paste0("Finished expanding community dates (time = ",round(time,2),")"))

# Calculate total probable cases over neighbours of each MSOA
comm_probable_expand %>%
  left_join(msoa_nb, by = c("msoa" = "neighbour_code")) %>%
  group_by(target_name, target_code, date) %>%
  summarise(probable_cases_nb = sum(probable_cases, na.rm = T)) %>%
  ungroup() %>%
  rename(msoa = target_code) %>%
  dplyr::select(msoa, date, probable_cases_nb) %>%
  as.data.frame() -> nb_prev

comm_probable_expand %>%
  lazy_dt() %>%
  left_join(nb_prev, by = c("msoa", "date")) %>%
  mutate(probable_cases_rate = probable_cases*1e5/tpp_pop,
         probable_cases_rate_nb = probable_cases_nb*1e5/tpp_pop) %>%
  as.data.frame() -> comm_prev

print("Summary: Daily community prevalence")
summary(comm_prev)

################################################################################
