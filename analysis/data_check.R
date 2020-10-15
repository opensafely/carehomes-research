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

pacman::p_load("tidyverse", "lubridate", "data.table", "dtplyr", "zoo")

sink("./data_checks.txt", type = "output")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# args = commandArgs(trailingOnly=TRUE)
args <- c("tpp_msoa_coverage.csv", "./data/msoa_shp.rds", "./output/input.csv")
# args = commandArgs(trailingOnly=TRUE)

tpp_cov <- fread(args[1], data.table = FALSE, na.strings = "") 

## Load shapefiles
msoa_shp <- readRDS(args[2])

input <- fread(args[3], data.table = FALSE, na.strings = "") %>%
  left_join(tpp_cov, by = "msoa") %>% 
  mutate(across(where(is.character), as.factor))

# Identify vars containing event dates
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")

# ---------------------------------------------------------------------------- #

hh_miss_msoa <- input %>%
  filter(is.na(msoa)) %>%
  pull(household_id) %>%
  n_distinct()
pat_miss_msoa <- input %>%
  filter(is.na(care_home_type)) %>%
  nrow()

hh_miss_type <- input %>%
  filter(is.na(care_home_type)) %>%
  pull(household_id) %>%
  n_distinct()
pat_miss_type <- input %>%
  filter(is.na(care_home_type)) %>%
  nrow()

pat_cov_miss <- input %>%
  filter((is.na(msoa) | is.na(care_home_type)) & any(!is.na(event_dates))) %>%
  nrow()

# HHs with missing MSOA: n = 
hh_miss_msoa

# Patients with missing HH MSOA: n = 
pat_miss_msoa

# HHs with missing type: n = 
hh_miss_type

# Patients with missing HH type: n = 
pat_miss_type

# COVID cases with missing MSOA or HH type: n = 
pat_cov_miss


# No. households and patients per carehome type
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n()) 

# probable prisons/institutions
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(household_id),
            n_pat = n()) 

# TPP coverage by MSOA
png("./tpp_coverage_map.png", height = 800, width = 800)
input %>%
  group_by(msoa) %>%
  summarise(tpp_cov = unique(tpp_cov)) %>%
  full_join(msoa_shp, by = c("msoa" = "MSOA11CD")) %>%
  ggplot(aes(geometry = geometry, fill = tpp_cov)) +
  geom_sf(lwd = 0) + 
  scale_fill_gradient2() +
  theme_minimal()
dev.off() 


# Care homes registered under > 1 system
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(household_id),
            n_pat = n()) 


sink()

