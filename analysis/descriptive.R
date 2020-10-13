################################################################################
# Description: Preliminary data summaries
# 
# Depends on data_setup.R and get_community_prevalence.R
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

sink("./log_descriptive.txt")

time_desc <- Sys.time()

################################################################################

pacman::p_load("tidyverse", "lubridate")
theme_set(theme_bw())

input <- readRDS("./input_clean.rds")
comm_prev <- readRDS("./community_prevalence.rds")
ch <- readRDS("./ch_linelist.rds")
ch_long <- readRDS("./ch_agg_long.rds")
dat <- readRDS("./analysisdata.rds")

################################################################################

## Care home characteristics: summary tables

# Total care homes in analysis dataset
N_ch_tot <- n_distinct(dplyr::select(dat, household_id, msoa))

# Number of carehomes per MSOA
ch %>% 
  group_by(msoa) %>% 
  summarise(
    n_resid = n(),
    n_ch = n_distinct(household_id),
    n_gp = n_distinct(practice_id)) %>%
  ungroup() -> per_msoa

write.csv(per_msoa, file = "./ch_gp_permsoa.csv", row.names = FALSE)

# Average number of carehomes per msoa
print("Summary: number of carehomes per MSOA")
per_msoa %>%
  pull(n_ch) %>%
  summary() 

#------------------------------------------------------------------------------#

# Summarise characteristics overall, and by whether or not care home had any 
# covid-event recorded in data (ever-affected). Size of each home estimated as 
# number of patients registered under that household ID, but actual capacity may 
# be larger. 
chars <- c("household_id","msoa","n_resid","ch_size","ch_type","rural_urban",
           "imd","hh_med_age","hh_p_female","hh_maj_ethn","hh_p_dem",
           "first_event", "ever_affected")

ch_chars <- ch_long %>%
  dplyr::select(all_of(chars)) %>%
  distinct()

# Missingness in care home characteristics:
ch_chars %>%
  summarise_all(function(x) sum(is.na(x))) 

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
            `No. residents` = sum(n_resid, na.rm = T),
            ch_size_mean = round(mean(ch_size, na.rm = T),2),
            ch_size_sd = round(sqrt(var(ch_size, na.rm = T)),2),
            `% rural` = round(sum(rural_urban == "rural")/N, 2),
            imd_mean = round(mean(imd, na.rm = T)),
            imd_sd = round(sqrt(var(imd, na.rm = T)),2)) %>%
  mutate(N_perc = round(N/N_ch_tot,2),
         # `N (%)` = paste0(N, " (",N_perc,")"),
         `size mean(sd)` = paste0(ch_size_mean, " (",ch_size_sd,")"),
         `IMD mean(sd)` = paste0(imd_mean, " (",imd_sd,")")) %>%
  ungroup() %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(N, `% rural`, `IMD mean(sd)`, `size mean(sd)`, `No. residents`) %>%
  cbind(tab_type) -> tab1

tab1

#------------------------------------------------------------------------------#

# Age, dementia status and ethnicity of care home residents, stratified by 
# whether or not their home was affected (percentages out of total residents in 
# that stratum):

ch_resid_all <- ch %>%
  mutate(ever_affected = "Overall")

ch %>%
  full_join(dplyr::select(ch_chars, household_id, msoa, ever_affected)) %>%
  mutate(ever_affected = ifelse(ever_affected,"Affected","Unaffected")) %>%
  bind_rows(ch_resid_all) %>% 
  mutate(ever_affected = factor(ever_affected, 
                                levels = c("Overall","Affected","Unaffected"))) -> ch_resid_all

ch_resid_all %>%
  group_by(ever_affected) %>%
  summarise(`No. residents` = n(),
            med_age = round(median(age, na.rm = T)),
            q_age = paste(round(quantile(age, 
                                         probs = c(0.25, 0.75), 
                                         na.rm = T)), collapse = ", "),
            n_dem = sum(dementia, na.rm = T)
  ) %>% 
  mutate(`age med[IQR]` = paste0(med_age, " [",q_age,"]"),
         `dementia n(%)` = paste0(n_dem, " (",round(n_dem/`No. residents`,2),")")) %>%
  ungroup() %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(`No. residents`, `age med[IQR]`, `dementia n(%)` ) -> tab_age

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
            function(x) paste0(x, " (", round(x/.$n_resid,2), ")")) %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(-n_resid) -> tab_ethn

tab2 <- cbind(tab_age, tab_ethn)
tab2

################################################################################
## FIGURES 
################################################################################
pdf(file = "./descriptive.pdf", height = 7, width = 9)

## Care home survival
# Cumulative care home survival

ch_long %>%
  group_by(date) %>%
  filter(first_event > date) %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(title = "Survival of care homes from COVID-19 introduction",
       x = "", y = "No. without event")

#------------------------------------------------------------------------------#

## Community burden
# Average daily incidence
comm_prev %>%
  group_by(date) %>%
  summarise(probable_cases_rate = mean(probable_cases_rate)) -> comm_prev_avg

# Community incidence over time
comm_prev %>%
  filter(date > ymd("2020-01-01")) %>%
  ggplot(aes(date, probable_cases_rate)) +
  geom_line(aes(group = msoa), alpha = 0.1) +
  geom_line(data = comm_prev_avg, col = "white", lty = "dashed", lwd = 1.5) + 
  labs(title = "Probable cases per 100,000, by MSOA",
       x = "", y = "Rate")

#------------------------------------------------------------------------------#

## Community incidence versus care home introduction
dat %>%
  mutate(event_ahead = as.factor(event_ahead)) %>%
  pivot_longer(c("probable_cases_rate","probable_chg7","probable_roll7")) %>%
  ggplot(aes(event_ahead, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(rows = "name", scales = "free") +
  labs(title = "Community incidence versus 14-day-ahead introduction",
       y = "Daily probable cases in community, per 100,000",
       x = "Introduction in next 14 days")

#------------------------------------------------------------------------------#

## Hospital discharges of care home residents
dat %>%
  group_by(date) %>%
  summarise(n_disch = sum(n_disch, na.rm = T)) %>%
  ggplot(aes(date, n_disch)) +
  geom_line() + 
  labs(title = "Hospital discharges of care home residents",
       x = "", y = "Count")

#------------------------------------------------------------------------------#

## Community, care home and older population epidemics
## Currently just absolute numbers as unsure of denominator for 70+ in community...
input %>%
  filter(!is.na(primary_care_case_probable) & primary_care_case_probable > ymd("2020-01-01")) %>%
  mutate(group = case_when(care_home_type == "U" & age < 70 ~ "Community",
                           care_home_type != "U" ~ "Care home",
                           care_home_type == "U" & age >= 70 ~ "Community, aged 70+")) %>%
  group_by(primary_care_case_probable, group) %>%
  count() %>% 
  ggplot(aes(primary_care_case_probable, n, col = group)) +
  geom_line() +
  labs(title = "Daily probable cases identified through primary care",
       col = "Population",
       x = "Date",
       y = "Count") + 
  theme(legend.position = c(0.2,0.8))

dev.off()


################################################################################

time_desc <- Sys.time() - time_desc
write(paste0("Total time running descriptive: ",round(time_desc,2)), file="log_descriptive.txt", append = TRUE)

sink() 

################################################################################