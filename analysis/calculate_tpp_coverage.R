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

pacman::p_load("tidyverse", "lubridate", "data.table", "dtplyr", "zoo")

# write("Calculating TPP coverage",file="coverage_log.txt")
sink("./coverage_log.txt", type = "output")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input_coverage.csv 
#   - household ID, size and MSOA for all TPP-registered patients
# * msoa_pop_allages.csv 
#   - total population estimates per MSOA

args <- c("./output/input_coverage.csv","./data/msoa_pops_allages.csv")
# args = commandArgs(trailingOnly=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  mutate(across(c("household_id", "msoa"), as.factor))
msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") 

# Sum household sizes across all unique household IDs
summary(input)

input %>%
  filter(!duplicated(household_id)) %>%
  group_by(msoa) %>%
  summarise(tpp_pop = sum(household_size, na.rm = T)) %>%
  full_join(msoa_pop) %>%
  mutate(tpp_cov = tpp_pop*100/pop) -> tpp_cov


saveRDS(tpp_cov, file = "./tpp_msoa_coverage.rds")

sink()