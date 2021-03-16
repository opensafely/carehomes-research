################################################################################
# Description: Preliminary data summaries
# 
# Generate summary tables and descriptive plots
# 
# Depends on data_setup.R and get_community_prevalence.R
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

sink("./log_descriptive.txt", type = "output")

time_desc <- Sys.time()

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(lubridate)
library(data.table)
library(sf)

theme_set(theme_bw())

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# Shapefiles
msoa_shp <- readRDS("./data/msoa_shp.rds")

# Individual data - all
input <- readRDS("./input_clean.rds")

# Community prevalence
comm_prev <- readRDS("./community_prevalence.rds")

# Individual data - care home residents
ch <- readRDS("./ch_linelist.rds")

# Aggregated data - daily care home events
ch_long <- readRDS("./ch_agg_long.rds")

# Analysis
dat <- readRDS("./analysisdata.rds")

study_per <- range(dat$date)

# ---------------------------------------------------------------------------- #

# Total care homes in analysis dataset
N_ch_tot <- n_distinct(dat$household_id)

print("Total included care homes:")
N_ch_tot

# Number of carehomes per MSOA
ch %>% 
  group_by(msoa) %>% 
  summarise(
    n_resid = n_distinct(patient_id),
    n_ch = n_distinct(household_id),
    n_gp = n_distinct(practice_id)) %>%
  ungroup() -> per_msoa

write.csv(per_msoa, file = "./ch_gp_permsoa.csv", row.names = FALSE)

print("Summary: number of carehomes per MSOA")
per_msoa %>%
  pull(n_ch) %>%
  summary() 

#------------------------------------------------------------------------------#

## Summary tables: care home characteristics ##

# Summarise characteristics overall, and by whether or not care home had any 
# covid-event recorded in data (ever-affected). Size of each home estimated as 
# number of patients registered under that household ID, but actual capacity may 
# be larger. 

chars <- c("household_id","msoa","n_resid","ch_size","ch_type","rural_urban",
           "imd","hh_med_age","hh_p_female","hh_prop_min","hh_p_dem",
           "first_event", "ever_affected")

included <- unique(dat$household_id)

dat %>%
  dplyr::select(all_of(chars)) %>%
  unique() -> ch_chars

print("Missingness in care home characteristics:")
print(
  ch_chars %>%
    summarise_all(function(x) sum(is.na(x))) %>%
    pivot_longer(cols = everything())
)

ch_overall <- ch_chars %>%
  mutate(ever_affected = "Overall")

chars_waffect <- ch_chars %>%
  mutate(ever_affected = ifelse(ever_affected, "Affected","Unaffected")) %>%
  bind_rows(ch_overall) %>% 
  mutate(ever_affected = factor(ever_affected, 
                                levels = c("Overall","Affected","Unaffected"))) 

# Tabulate care home type (factor):
chars_waffect %>%
  group_by(ever_affected, ch_type) %>%
  summarise(n_resid = n()) %>%
  ungroup() %>%
  mutate(ch_type = replace_na(as.character(ch_type), "missing")) %>%
  pivot_wider(names_from = ch_type, values_from = n_resid, names_prefix = "Type:") %>%
  rowwise() %>%
  mutate(n_resid = sum(c_across(cols = -ever_affected))) %>% 
  ungroup() %>%
  mutate_at(vars(-n_resid, -ever_affected), 
            function(x) paste0(x, " (", round(x/.$n_resid,2), ")")) %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(-n_resid) -> tab_type

# Tabulate other variables
chars_waffect %>%
  group_by(ever_affected) %>%
  summarise(N = n(),
            `No. TPP residents` = sum(n_resid, na.rm = T),
            ch_size_mean = round(mean(ch_size, na.rm = T),2),
            ch_size_sd = round(sqrt(var(ch_size, na.rm = T)),2),
            # `% rural` = round(sum(rural_urban == "rural")/N, 2),
            imd_mean= round(mean(imd, na.rm = T)),
            imd_sd= round(sqrt(var(imd, na.rm = T))),
            imd_quants = paste(round(quantile(imd, 
                                              probs = c(0.25, 0.75), 
                                              na.rm = T)), collapse = ", "),
            ) %>%
  mutate(N_perc = round(N/N_ch_tot,2),
         # `N (%)` = paste0(N, " (",N_perc,")"),
         `size mean(sd)` = paste0(ch_size_mean, " (",ch_size_sd,")"),
         `IMD mean(sd)` = paste0(imd_mean, " (",imd_sd,")")) %>% 
  ungroup() %>% 
  remove_rownames() %>%
  column_to_rownames(var = "ever_affected") %>%
  dplyr::select(N, `IMD mean(sd)`, `size mean(sd)`, `No. TPP residents`) %>% #`% rural`,
  cbind(tab_type) -> tab1

print("Summarise carehome characteristics by ever affected:")
tab1

#------------------------------------------------------------------------------#

# Age, dementia status and ethnicity of care home residents, stratified by 
# whether or not their home was affected (percentages out of total residents in 
# that stratum):

ch_resid_all <- ch %>%
  mutate(ever_affected = "Overall")

ch %>%
  right_join(dplyr::select(ch_chars, household_id, msoa, ever_affected)) %>% 
  mutate(ever_affected = ifelse(ever_affected,"Affected","Unaffected")) %>%
  bind_rows(ch_resid_all) %>% 
  mutate(ever_affected = factor(ever_affected, 
                                levels = c("Overall","Affected","Unaffected"))) -> ch_resid_all

ch_resid_all %>%
  group_by(ever_affected) %>%
  summarise(`No. TPP residents` = n(),
            med_age = round(median(age, na.rm = T)),
            q_age = paste(round(quantile(age, 
                                         probs = c(0.25, 0.75), 
                                         na.rm = T)), collapse = ", "),
            n_minor = sum(ethnicity != 1, na.rm = T),
            prop_minor = mean(ethnicity != 1, na.rm = T),
            n_dem = sum(dementia, na.rm = T)
  ) %>% 
  mutate(`age med[IQR]` = paste0(med_age, " [",q_age,"]"),
         `minority ethnicity n(%)` = paste0(n_minor, " (",round(n_minor/`No. TPP residents`,4),")"),
         `dementia n(%)` = paste0(n_dem, " (",round(n_dem/`No. TPP residents`,4),")")) %>%
  ungroup() %>%
  remove_rownames() %>% 
  column_to_rownames("ever_affected") %>%
  dplyr::select(`No. TPP residents`, `age med[IQR]`, `minority ethnicity n(%)`, `dementia n(%)` ) -> tab_age

ch_resid_all %>%
  group_by(ever_affected, ethnicity) %>%
  summarise(n_resid = n()) %>%
  ungroup() %>%
  mutate(ethnicity = replace_na(as.character(ethnicity), "missing")) %>%
  pivot_wider(names_from = ethnicity, values_from = n_resid, names_prefix = "Ethn:") %>%
  rowwise() %>%
  mutate(n_resid = sum(c_across(cols = -ever_affected))) %>% 
  ungroup() %>%
  mutate_at(vars(-n_resid, -ever_affected), 
            function(x) paste0(x, " (", round(x/.$n_resid,4), ")")) %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(-n_resid) -> tab_ethn

tab2 <- cbind(tab_age, tab_ethn)

print("Summarise resident characteristics by ever affected:")
tab2

################################################################################
## FIGURES 
################################################################################

pdf(file = "./descriptive.pdf", height = 7, width = 9)

## Age distribution
# png("./age_histogram.png", height = 600, width = 800)
ggplot(input, aes(age)) +
  geom_histogram() +
  facet_wrap(~ care_home_type, scales = "free_y")
# dev.off()

## Care home survival
# Cumulative care home survival

# png("./ch_survival.png", height = 500, width = 500)
ch_long %>%
  group_by(date) %>%
  filter(first_event > date) %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(title = "Survival of care homes from COVID-19 introduction",
       x = "", y = "No. without event")
# dev.off()

ch_long %>%
  group_by(date, ch_type) %>%
  filter(first_event > date & ch_type != "PS") %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n, col = ch_type)) +
  geom_line() +
  # facet_wrap(~ch_type, scales = "free_y") +
  labs(title = "Survival of care homes from COVID-19 introduction",
       x = "", y = "No. without event", col = "Type")

# Type of first event
# png("./ch_first_event_type.png", height = 800, width = 1000)
ch_long %>%
  filter(ever_affected) %>%
  ggplot(aes(first_event, fill = first_event_which)) +
  geom_histogram() + 
  theme_minimal() +
  theme(legend.position = c(0.8,0.8))
# dev.off()

ch_long %>%
  group_by(msoa, household_id) %>%
  summarise(ever_affected = unique(ever_affected)) %>%
  group_by(msoa) %>%
  summarise(affect_prop = mean(as.numeric(ever_affected))) -> affect_bymsoa

# png("./ch_first_event_map.png", height = 1000, width = 1000, res = 150)
# msoa_shp %>%
#   full_join(affect_bymsoa, by = c("MSOA11CD" = "msoa")) %>%
#   ggplot(aes(geometry = geometry, fill = affect_prop)) +
#   geom_sf(lwd = 0) +
#   labs(title = "Proportion of TPP-covered carehomes ever affected during study period",
#        fill = "Proportion") +
#   scale_fill_viridis_c() +
#   theme(legend.position = c(0.2,0.8))
# dev.off()


# ch_long %>%
#   filter(ever_affected == TRUE) %>%
#   group_by(msoa, household_id) %>%
#   summarise(first_event = unique(first_event)) %>%
#   group_by(msoa) %>%
#   summarise(average_first_event = median(first_event, na.rm = TRUE),
#             first_event = min(first_event)) -> first_bymsoa

# msoa_shp %>%
#   full_join(first_bymsoa, by = c("MSOA11CD" = "msoa")) %>%
#   ggplot(aes(geometry = geometry, fill = average_first_event)) +
#   geom_sf(lwd = 0) +
#   labs(title = "Average timing of first care home event per MSOA",
#        fill = "Date of first event") +
#   scale_fill_viridis_c() +
#   theme(legend.position = c(0.2,0.8))

# msoa_shp %>%
#   full_join(first_bymsoa, by = c("MSOA11CD" = "msoa")) %>%
#   ggplot(aes(geometry = geometry, fill = first_event)) +
#   geom_sf(lwd = 0) +
#   labs(title = "First care home event per MSOA",
#        fill = "Date of first event") +
#   scale_fill_viridis_c() +
#   theme(legend.position = c(0.2,0.8))

#------------------------------------------------------------------------------#

## Community burden
# Average daily incidence
dat %>%
  group_by(date) %>%
  summarise(probable_roll7 = mean(probable_roll7, na.rm = T)) %>%
  ungroup() -> comm_prev_avg

# Community incidence over time
# png("./community_inc.png", height = 500, width = 500)
dat %>%
  ggplot(aes(date, probable_roll7)) +
  geom_line(aes(group = msoa), alpha = 0.1) +
  geom_line(data = comm_prev_avg, col = "white", lty = "dashed", lwd = 1.5) + 
  labs(title = "Probable cases per 100,000, by MSOA",
       subtitle = "Rolling seven day mean",
       x = "", y = "Rate") +
  scale_x_date(limits = study_per)
# dev.off()

#------------------------------------------------------------------------------#

## Community incidence versus care home introduction
# png("./comm_vs_ch.png", height = 800, width = 800)
dat %>%
  mutate(event_ahead = as.factor(event_ahead)) %>%
  pivot_longer(c("probable_cases_rate","probable_roll7","probable_roll7_lag1wk","probable_roll7_lag2wk")) %>%
  ggplot(aes(event_ahead, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(rows = "name", scales = "free") +
  labs(title = "Community incidence versus 14-day-ahead introduction",
       y = "Daily probable cases in community, per 100,000",
       x = "Introduction in next 14 days")
# dev.off()

#------------------------------------------------------------------------------#

## Hospital discharges of care home residents
# png("./discharges.png", height = 500, width = 500)
# dat %>%
#   group_by(date) %>%
#   summarise(n_disch = sum(n_disch, na.rm = T)) %>%
#   ggplot(aes(date, n_disch)) +
#   geom_line() + 
#   labs(title = "Total hospital discharges of care home residents",
#        x = "", y = "Count")
# dev.off()

#------------------------------------------------------------------------------#

## Community, care home and older population epidemics
## Currently just absolute numbers as don't have denominator of population in 
## community and carehome per MSOA

# png("./compare_epidemics.png", height = 500, width = 500)
input %>%
  filter(!is.na(primary_care_case_probable) & primary_care_case_probable > ymd("2020-01-01")) %>%
  mutate(group = case_when(care_home_type == "U" & age < 70 ~ "Community",
                           care_home_type != "U" ~ "Care home",
                           care_home_type == "U" & age >= 70 ~ "Community, aged 70+")) %>%
  group_by(primary_care_case_probable, group) %>%
  summarise(n = n()) %>% #, msoa_pop = unique(msoa_pop), pop_gt70 = unique(`70+`)
  ggplot(aes(primary_care_case_probable, n, col = group)) +
  geom_line() +
  labs(title = "Daily probable cases identified through primary care",
       col = "Population",
       x = "Date",
       y = "Count") + 
  theme(legend.position = c(0.2,0.8))
# dev.off()

dev.off()

################################################################################

time_desc <- Sys.time() - time_desc
write(paste0("Total time running descriptive: ",round(time_desc,2)), file="log_descriptive.txt", append = TRUE)

sink() 

################################################################################
