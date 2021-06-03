################################################################################
# Description: Script to calculate TPP coverage per MSOA according to household
# size of TPP-registered patients and ONS population estimates per MSOA
#
# input: 
#
# Author: Emily S Nightingale
# Date: 13/10/2020
#
################################################################################

time_total <- Sys.time()

################################################################################

library(tidyverse)
library(data.table)
library(dtplyr)

# write("Calculating TPP coverage",file="coverage_log.txt")
sink("./coverage_log.txt", type = "output")

options(datatable.old.fread.datetime.character = TRUE)

# ---------------------------------------------------------------------------- #

#----------------------#
#    LOAD/CLEAN DATA   #
#----------------------#

# * input.csv 
#   - pull household ID, size and MSOA for all TPP-registered patients
# * msoa_pop.csv 
#   - total population estimates per MSOA
#   - population estimates by single year age
# 
# args <- c("./input.csv","./data/SAPE22DT15_mid_2019_msoa.csv", 600)
args = commandArgs(trailingOnly = TRUE)

## TPP-registered patient records (from study definition)
## Include ALL patients with non-missing MSOA in calculation of TPP populations
input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # Remove individuals w missing/non-England MSOA
  filter(grepl("E",msoa) & !is.na(msoa))

## National MSOA population estimates (ONS mid-2019):
msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") %>%
  mutate(msoa = as.factor(`MSOA Code`),
         msoa_pop = `All Ages`) %>%
  # Filter to England
  filter(grepl("E", msoa)) %>%
  # Sum older populations
  rowwise() %>%
  mutate(`70+` = sum(`70-74`:`90+`)) %>%
  dplyr::select(msoa, msoa_pop, `70+`) %>%
  ungroup()

## MSOA TPP coverage cut off
msoa_cov_cutoff <- as.numeric(args[3])

# ---------------------------------------------------------------------------- #

print("No. MSOAs in England:")
n_distinct(msoa_pop$msoa)

print("No. TPP-registered patients with non-missing MSOA:")
nrow(input)

print("No. unique MSOAs with patients registered in TPP:")
n_distinct(input$msoa)

print("No. TPP-registered patients with non-missing MSOA and non-missing HHID:")
nrow(input[input$household_id > 0,])

print("No. unique MSOAs with patients registered in TPP and non-missing HHID:")
n_distinct(input$msoa[input$household_id > 0])

print("Count probable cases with/without HHID:")
input %>%
  group_by((household_id > 0)) %>%
  summarise(n_probable = sum(!is.na(primary_care_case_probable)))

# ---------------------------------------------------------------------------- #

#------------------------------------------#
#  Aggregate by MSOA and merge with pops   #
#------------------------------------------#

input %>%
  # Count records per MSOA
  group_by(msoa) %>%
  tally(name =  "tpp_pop_all") %>%
  ungroup() -> tpp_pop_all

input %>%
  filter(household_id > 0) %>%
  # Count records per MSOA
  group_by(msoa) %>%
  tally(name =  "tpp_pop_wHHID") %>%
  ungroup() -> tpp_pop_wHHID

tpp_pop_all %>%
  full_join(tpp_pop_wHHID) %>%
  # Merge MSOAs in OS with total population estimates
  left_join(msoa_pop) %>%
  mutate(msoa = as.factor(msoa),
         tpp_cov_all = tpp_pop_all*100/msoa_pop,
         tpp_cov_wHHID = tpp_pop_wHHID*100/msoa_pop,
         cov_gt_100 = as.factor(ifelse(tpp_cov_all > 100, "Yes", "No"))) -> tpp_cov

summary(tpp_cov)

over100 <- filter(tpp_cov, cov_gt_100 == "Yes")
write.csv(over100, "./msoa_gt_100_cov.csv", row.names = FALSE)

tpp_msoas <- unique(input$msoa)
write.csv(tpp_msoas, "./msoas_in_tpp.csv", row.names = FALSE)

# Figure
png("./total_vs_tpp_pop.png", height = 800, width = 800)
tpp_cov %>%
  pivot_longer(c("tpp_cov_all","tpp_cov_wHHID")) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~name) +
  labs(x = "TPP coverage per MSOA") +
  theme_minimal()
dev.off() 

# ---------------------------------------------------------------------------- #

#------------------------------------------#
#    Filter on coverage cutoff and save    #
#------------------------------------------#

# Filter MSOAs by TPP coverage
exclude_msoa <- tpp_cov %>%
  filter(tpp_cov_wHHID < msoa_cov_cutoff) %>%
  pull(msoa) %>%
  unique()

print(paste0("MSOAs excluded with ",msoa_cov_cutoff,"% coverage cut off: n = ",length(exclude_msoa)))

tpp_cov_incl <- tpp_cov %>%
  filter(tpp_cov_wHHID >= msoa_cov_cutoff)

print(paste0("MSOAs included with ",msoa_cov_cutoff,"% coverage cut off: n = ",nrow(tpp_cov_incl)))

summary(tpp_cov_incl$tpp_cov_wHHID)

png("./tpp_cov_filtered.png", height = 800, width = 800)
tpp_cov_incl %>%
  ggplot(aes(tpp_cov_wHHID)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~name) +
  labs(x = "TPP coverage per MSOA - filtered on 80% cutoff") +
  theme_minimal()
dev.off() 

# Trying to find where the cov = 9% come from...
tpp_cov_incl %>%
  mutate(low_cov = (tpp_cov_wHHID < msoa_cov_cutoff)) %>%
  group_by(low_cov) %>%
  tally()

tpp_cov_incl %>%
  filter(tpp_cov_wHHID < msoa_cov_cutoff)

saveRDS(tpp_cov_incl, file = "./tpp_coverage_included.rds")
saveRDS(tpp_cov, file = "./tpp_coverage_all.rds")
write.csv(tpp_cov, "./tpp_coverage_all.csv", row.names = FALSE)

################################################################################

sink()

################################################################################
################################################################################
