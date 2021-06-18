################################################################################
# Description: Summary figures to sense-check cleaned data 
#
# input: Cleaned input data from data_clean.R
#
# Author: Emily S Nightingale
# Date: 01/10/2020
#
################################################################################

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)

theme_set(theme_minimal())

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# * input_clean.csv 
#   - individual health records for identification of covid events

# args <- c("input_clean.rds", "data/msoa_shp.rds")
args = commandArgs(trailingOnly = TRUE)

input <- readRDS(args[1]) %>%
  rename(tpp_cov = tpp_cov_wHHID)

## Load shapefiles
msoa_shp <- readRDS(args[2])

# ---------------------------------------------------------------------------- #

#----------------------#
#       FIGURES        #
#----------------------#

# Distribution of household size, 
png("./hh_size_dist.png", height = 800, width = 1400)
input %>%
  filter(household_size < quantile(household_size, 0.999, na.rm = T)) %>%
  group_by(care_home_type, household_id) %>%
  summarise(household_size = unique(household_size)) %>%
  ggplot(aes(household_size)) +
  geom_histogram(fill = "white", col = "black") +
  facet_wrap(~care_home_type, scales = "free") 
dev.off()

# Age distribution in carehomes/community
png("./age_dist.png", height = 800, width = 1400)
input %>%
  mutate(group = case_when(ch_ge65 == T ~ "Care home", 
                           ch_ge65 == F ~ "Community")) %>%
  ggplot(aes(age)) +
  geom_histogram(bins = 30, fill = "white", col = "black") +
  geom_vline(aes(xintercept = median(age, na.rm = T)), col = "red") +
  facet_wrap(~group, scales = "free") 
dev.off()

# TPP coverage by MSOA
png("./tpp_coverage_msoa.png", height = 500, width = 600)
input %>%
  group_by(msoa) %>%
  summarise(tpp_cov = unique(tpp_cov)) %>%
  ggplot(aes(tpp_cov)) +
  geom_histogram(bins = 30, fill = "white", col = "black") 
dev.off()

png("./tpp_coverage_carehomes.png", height = 500, width = 600)
input %>%
  filter(care_home_type != "U") %>%
  dplyr::select(household_id, percent_tpp) %>%
  unique() %>% 
  ggplot(aes(percent_tpp)) +
  geom_histogram(bins = 30, fill = "white", col = "black") 
dev.off() 

# input %>%
#   group_by(msoa) %>%
#   summarise(tpp_cov = mean(tpp_cov, na.rm = T)) %>%
#   ungroup() -> by_msoa
# 
# pdf("./tpp_coverage_map.pdf", height = 10, width = 8)
# msoa_shp %>% 
#   full_join(by_msoa, by = c("MSOA11CD" = "msoa")) %>%
#   ggplot(aes(geometry = geometry, fill = tpp_cov)) +
#   geom_sf(lwd = 0) +
#   scale_fill_gradient2(midpoint = 100, low = "steelblue", high = "indianred", mid = "white") +
#   theme(legend.position = c(0.2,0.9)) 
# dev.off()

png("./infection_death_delays.png", height = 500, width = 900)
input %>%
  filter(care_home_type != "U") %>%
  pivot_longer(prob_death_delay:test_death_delay) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "white", col = "black") +
  facet_wrap(~name, scales = "free")
dev.off()

# ---------------------------------------------------------------------------- #

