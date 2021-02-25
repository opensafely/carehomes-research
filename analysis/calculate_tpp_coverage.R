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

options(datatable.old.fread.datetime.character=TRUE)

# ---------------------------------------------------------------------------- #

#----------------------#
#    LOAD/CLEAN DATA   #
#----------------------#

# * input_coverage.csv 
#   - household ID, size and MSOA for all TPP-registered patients
# * msoa_pop.csv 
#   - total population estimates per MSOA
#   - population estimates by single year age

# args <- c("./input.csv","./data/SAPE22DT15_mid_2019_msoa.csv")
args = commandArgs(trailingOnly=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  # Remove individuals w missing MSOA/HHID/HH size
  filter(!is.na(msoa) & household_id > 0) %>%
  mutate(msoa = as.factor(msoa)) %>%
  # Keep one row per household to sum sizes 
  # HH size should only be missing if missing for ALL residents 
  dplyr::select(-patient_id) %>%
  group_by(household_id) %>%
  # Select largest size among all residents
  slice_max(household_size, n = 1, with_ties = FALSE) %>%
  ungroup()

print("No. unique MSOAs with patients registered in TPP:")
n_distinct(input$msoa)

print("No. unique households with missing size (size missing for all residents):")
n_distinct(input$household_id[input$household_size <= 0])

print("MSOAs of households with missing size:")
unique(input$msoa[input$household_size <= 0])

print("No. rows per household ID:")
input %>% 
  group_by(household_id) %>%
  tally() %>%
  pull(n) %>%
  summary()

# ---------------------------------------------------------------------------- #

#------------------------------------------#
#  Aggregate by MSOA and merge with pops   #
#------------------------------------------#

input %>%
  group_by(msoa) %>%
  summarise(tpp_pop = sum(household_size, na.rm = TRUE)) -> tpp_pop

msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") %>%
  mutate(msoa = as.factor(`MSOA Code`),
         msoa_pop = `All Ages`) %>%
  rowwise() %>%
  mutate(`70+` = sum(`70-74`:`90+`)) %>%
  dplyr::select(msoa, msoa_pop, `70+`) %>%
  ungroup()

print("No. MSOAs in England & Wales:")
n_distinct(msoa_pop$msoa)

tpp_pop %>%
  # Merge MSOAs in OS with total population estimates
  left_join(msoa_pop) %>%
  mutate(tpp_cov = tpp_pop*100/msoa_pop,
         cov_gt_100 = as.factor(ifelse(tpp_cov > 100, "Yes", "No"))) -> tpp_cov

summary(tpp_cov)

over100 <- filter(tpp_cov, cov_gt_100 == "Yes")
write.csv(over100, "./msoa_gt_100_cov.csv", row.names = FALSE)

png("./total_vs_tpp_pop.png", height = 800, width = 800)
tpp_cov %>%
  ggplot(aes(tpp_cov)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off() 

saveRDS(tpp_cov, file = "./tpp_msoa_coverage.rds")
write.csv(tpp_cov, "./tpp_msoa_coverage.csv", row.names = FALSE)

tpp_msoas <- unique(input$msoa)
write.csv(tpp_msoas, "./msoas_in_tpp.csv", row.names = FALSE)

sink()
