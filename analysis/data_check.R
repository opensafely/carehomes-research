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

library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)
library(ggplot2)
library(sf)

sink("./data_checks.txt", type = "output")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input.csv 
#   - individual health records for identification of covid events
# * community_prevalence.csv 
#   - derived dataset of daily probable case counts per MSOA plus population estimates

# Identify vars containing event dates
event_dates <- c("primary_care_case_probable","first_pos_test_sgss","covid_admission_date", "ons_covid_death_date")

# args <- c("input.csv","tpp_msoa_coverage.rds", "data/msoa_shp.rds", 2)
args = commandArgs(trailingOnly=TRUE)

## Load MSOA TPP coverage
tpp_cov <- readRDS(args[2])

## Load shapefiles
msoa_shp <- readRDS(args[3])

## MSOA TPP coverage cut off
cutoff <- args[4]

options(datatable.old.fread.datetime.character=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # Filter just to records from England
  filter(grepl("E",msoa)) %>%
  # Redefine household_id to ensure unique across MSOAs
  group_by(msoa, household_id) %>%
  mutate(HHID = row_number()) %>% 
  ungroup() %>%
  inner_join(tpp_cov, by = "msoa") %>% 
  rowwise() %>%
  mutate(case = any(!is.na(c_across(all_of(event_dates))))) %>%
  ungroup() %>%
  mutate(across(all_of(event_dates), ymd),
         across(where(is.character), as.factor),
         test_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss),
         prob_death_delay = as.integer(ons_covid_death_date - first_pos_test_sgss)) 

# ---------------------------------------------------------------------------- #

# Filter MSOAs by TPP coverage

input_covcutoff <- input %>%
  filter(tpp_cov > cutoff)

print(paste0("MSOAs excluded with ",cutoff,"% coverage cut off: n = ",n_distinct(input$msoa)-n_distinct(input_covcutoff$msoa)))

input <- input_covcutoff

print("Total Patients")
n_distinct(input$patient_id)

print("Patients with missing HH MSOA:")
summary(is.na(input$msoa))

print("Patients with missing HH type:")
summary(is.na(input$care_home_type))

print("HHs with missing MSOA: n = ")
input %>%
  filter(is.na(msoa)) %>%
  pull(HHID) %>%
  n_distinct()

print("HHs with missing type: n = ")
input %>%
  filter(is.na(care_home_type)) %>%
  pull(HHID) %>%
  n_distinct()

print("COVID cases with missing MSOA or HH type: n = ")
input %>%
  filter(is.na(msoa) | is.na(care_home_type)) %>%
  rowwise() %>%
  filter(any(!is.na(c_across(all_of(event_dates))))) %>%
  pull(patient_id) %>%
  n_distinct()


print("No. households, patients and probable cases per carehome type:")
input %>%
  group_by(care_home_type) %>%
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# Distribution of household size, 
png("./hh_size_dist.png", height = 800, width = 1500)
input %>%
  group_by(care_home_type, HHID) %>%
  summarise(household_size = median(household_size, na.rm = T)) %>%
  ggplot(aes(household_size)) +
  geom_histogram() +
  facet_wrap(~care_home_type, scales = "free") +
  theme_minimal()
dev.off()

print("Probable prisons/institutions (size>15 and not CH)")
input %>%
  mutate(institution = (care_home_type == "U" & household_size > 15)) %>%
  group_by(institution) %>%
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

# Age distribution in carehomes/community
png("./age_dist.png", height = 800, width = 1500)
input %>%
  mutate(group = case_when(care_home_type == "U" ~ "Community",
                           care_home_type !="U" ~ "Care home")) %>%
  ggplot(aes(age)) +
  geom_histogram(bins = 30) +
  facet_wrap(~group) +
  theme_minimal()
dev.off()

# TPP coverage by MSOA
png("./tpp_coverage_msoa.png", height = 800, width = 800)
input %>%
  group_by(msoa) %>%
  summarise(tpp_cov = unique(tpp_cov)) %>%
  ggplot(aes(tpp_cov)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off()

print("Care homes registered under > 1 system:")
input %>%
  filter(care_home_type != "U") %>%
  mutate(mixed_household = replace_na(mixed_household, 0)) %>% 
  group_by(mixed_household) %>% 
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes with < 100% coverage:")
input %>%
  filter(care_home_type != "U") %>%
  group_by(percent_tpp < 100) %>% 
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE)) 

print("Care homes % TPP coverage:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(msoa, HHID, percent_tpp) %>%
  unique() %>% 
  pull(percent_tpp)
)

print("Care homes % TPP coverage category:")
summary(
  input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(msoa, HHID, percent_tpp) %>%
  unique() %>% 
  mutate(percent_tpp_cat = cut(percent_tpp, 
                               breaks = c(min(percent_tpp),50,60,70,80,90,max(percent_tpp)),
                               include.lowest = TRUE)) %>%
  pull(percent_tpp_cat)
)

input %>%
  filter(care_home_type != "U") %>%
  mutate(percent_tpp_cat = cut(percent_tpp, 5)) %>%
  group_by(percent_tpp_cat) %>%
  summarise(n_hh = n_distinct(HHID),
            n_pat = n_distinct(patient_id),
            n_case = sum(case, na.rm = TRUE))


png("./tpp_coverage_carehomes.png", height = 800, width = 800)
input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(HHID, percent_tpp) %>%
  unique() %>% 
  ggplot(aes(percent_tpp)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off() 

input %>%
  group_by(msoa) %>%
  summarise(tpp_cov = mean(tpp_cov, na.rm = T)) -> by_msoa
pdf("./tpp_coverage_map.pdf", height = 10, width = 8)
msoa_shp %>% 
  full_join(by_msoa, by = c("MSOA11CD" = "msoa")) %>%
  ggplot(aes(geometry = geometry, fill = tpp_cov)) +
  geom_sf(lwd = 0) +
  scale_fill_gradient2(midpoint = 100) +
  theme(legend.position = c(0.2,0.9)) +
  theme_minimal()
dev.off()

print("Care home residents test-diagnosis delay")
summary(
input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(prob_death_delay, test_death_delay) 
)
  

png("./infection_death_delays.png", height = 800, width = 1200)
input %>%
  filter(care_home_type != "U") %>%
  pivot_longer(prob_death_delay:test_death_delay) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~name) +
  theme_minimal()
dev.off() 

sink()

