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

shp <- sf::st_read(dsn = "./data/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC",  
                   layer = "Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC")

# ---------------------------------------------------------------------------- #

#----------------------#
#  LOAD DATA  #
#----------------------#

# * input_coverage.csv 
#   - household ID, size and MSOA for all TPP-registered patients
# * msoa_pop.csv 
#   - total population estimates per MSOA
#   - population estimates by single year age

# args <- c("./output\/input_coverage.csv","./data/msoa_pop.csv") 
args = commandArgs(trailingOnly=TRUE)

input <- fread(args[1], data.table = FALSE, na.strings = "") %>%
  mutate(msoa = as.factor(msoa))

msoa_pop <- fread(args[2], data.table = FALSE, na.strings = "") %>%
  rename(msoa = `Area Codes`,
         msoa_pop = `All Ages`) %>%
  rowwise() %>%
  mutate(`70+` = sum(`70`:`90+`)) %>%
  dplyr::select(msoa, msoa_pop, `70+`)

# Sum household sizes across all unique household IDs
summary(input)

input %>%
  group_by(msoa) %>%
  count(name = "tpp_pop") %>%
  full_join(msoa_pop) %>%
  mutate(tpp_cov = tpp_pop*100/msoa_pop) -> tpp_cov


shp %>%
  full_join(shp, by = c("MSOA11CD" = "msoa")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = tpp_cov)) +
  theme_bw() -> map_cov

pdf("./map_coverage_msoa.pdf", height = 10, width = 10)
map_cov
dev.off()

saveRDS(tpp_cov, file = "./tpp_msoa_coverage.rds")
write.csv(tpp_cov, "./tpp_msoa_coverage.csv", row.names = FALSE)

sink()
