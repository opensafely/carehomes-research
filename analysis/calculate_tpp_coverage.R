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

# shp <- sf::st_read(dsn = "./data/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC",  
#                    layer = "Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input_coverage.csv 
#   - household ID, size and MSOA for all TPP-registered patients
# * msoa_pop.csv 
#   - total population estimates per MSOA
#   - population estimates by single year age

# args <- c("./output/input_coverage.csv","./data/SAPE22DT15_mid_2019_msoa.csv")
args = commandArgs(trailingOnly=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  dplyr::select(-patient_id) %>%
  filter(!is.na(msoa)) %>%
  mutate(msoa = as.factor(msoa)) %>%
  group_by(household_id) %>%
  #keep one row per household to sum sizes
  slice_head()

print("No. unique MSOAs with patients registered in TPP:")
n_distinct(input$msoa)

msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") %>%
  mutate(msoa = as.factor(`MSOA Code`),
         msoa_pop = `All Ages`) %>%
  filter(grepl("E", msoa)) %>%
  rowwise() %>%
  mutate(`70+` = sum(`70-74`:`90+`)) %>%
  dplyr::select(msoa, msoa_pop, `70+`) %>%
  ungroup()

print("No. MSOAs in England:")
n_distinct(msoa_pop$msoa)

input %>%
  group_by(msoa) %>%
  summarise(tpp_pop = sum(household_size, na.rm = TRUE)) %>%
  inner_join(msoa_pop) %>%
  mutate(tpp_cov = tpp_pop*100/msoa_pop) -> tpp_cov

summary(tpp_cov)

png("./total_vs_tpp_pop.png", height = 800, width = 800)
tpp_cov %>%
  ggplot(aes(tpp_cov)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  theme_minimal()
dev.off() 

# shp %>%
#   full_join(shp, by = c("MSOA11CD" = "msoa")) %>%
#   ggplot() +
#   geom_sf(aes(geometry = geometry, fill = tpp_cov)) +
#   theme_bw() -> map_cov
# 
# pdf("./map_coverage_msoa.pdf", height = 10, width = 10)
# map_cov
# dev.off()

saveRDS(tpp_cov, file = "./tpp_msoa_coverage.rds")
write.csv(tpp_cov, "./tpp_msoa_coverage.csv", row.names = FALSE)

tpp_msoas <- unique(input$msoa)
write.csv(tpp_msoas, "./msoas_in_tpp.csv", row.names = FALSE)

sink()
