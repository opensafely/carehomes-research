################################################################################
# Description: Preliminary data summaries
# 
# Generate summary tables and descriptive plots
# 
# Depends on data_setup.R and get_community_incidence.R
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

sink("./log_descriptive.txt")

time_desc <- Sys.time()

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(lubridate)
library(data.table)

theme_set(theme_minimal())

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# Cleaned input
input <- readRDS("./input_clean.rds")

# Community prevalence
comm_inc <- readRDS("./community_incidence.rds")

# Individual data - care home residents
ch <- readRDS("./ch_linelist.rds") 

# Aggregated data - daily care home events
ch_long <- readRDS("./ch_agg_long.rds")

# Analysis
dat <- readRDS("./analysisdata.rds")

study_per <- range(dat$date)

event_dates <- c("primary_care_case_probable","first_pos_test_sgss",
                 "covid_admission_date", "ons_covid_death_date")

# ---------------------------------------------------------------------------- #

#--------------------------------------------------------#
#      TALLY CARE HOMES/RESIDENTS/PRACTICES/EVENTS       #
#--------------------------------------------------------#

# Total care homes in analysis dataset
N_ch_tot <- n_distinct(dat$household_id)

print("Total included care homes:")
N_ch_tot

# Counts per MSOA
ch %>% 
  group_by(msoa) %>% 
  summarise(
    n_resid = n_distinct(patient_id),
    n_ch = n_distinct(household_id),
    n_gp = n_distinct(practice_id)) %>%
  ungroup() -> per_msoa

write.csv(per_msoa, file = "./ch_gp_permsoa.csv", row.names = FALSE)

print("No. care homes per MSOA")
per_msoa %>%
  pull(n_ch) %>%
  summary() 

# Covid events
print("No. events among care home residents:")
ch %>%
  summarise(across(event_dates, function(x) sum(!is.na(x))))

# Unique affected residents
print("No. residents with >= 1 event:")
summary(ch$case)

print("No. events per home:")
ch %>%
  filter(case) %>%
  group_by(household_id) %>%
  summarise(N_case = n()) -> events_per_home
summary(events_per_home$N_case)

#------------------------------------------------------------------------------#

#---------------------------------------------#
#        COMMUNITY INC VS INTRODUCTION        #
#---------------------------------------------#

print("Summary: community incidence by occurrence of a care home event:")
dat %>% 
  pivot_longer(c("msoa_roll7","msoa_lag1wk","msoa_lag2wk",
                 "eng_roll7","eng_lag1wk","eng_lag2wk")) %>%
  group_by(event_ahead, name) %>%
  summarise(min = min(value, na.rm = T), 
            max = max(value, na.rm = T), 
            mean = mean(value, na.rm = T), 
            sd = sqrt(var(value, na.rm = T)), 
            med = median(value, na.rm = T))

#------------------------------------------------------------------------------#

#----------------------#
#        TABLES        #
#----------------------#

# Characteristics of interest
chars <- c("household_id","msoa","n_resid","ch_size","ch_type","rural_urban", "imd",
           "hh_med_age","hh_p_female","hh_p_min","hh_p_dem", "hh_maj_dem", 
           "n_case", "first_event", "first_event_which", "ever_affected")

# One row per home from analysis dataset
dat %>%
  dplyr::select(all_of(chars)) %>%
  unique() -> ch_chars

# Check missingness
print("Missingness in care home characteristics:")
print(
  ch_chars %>%
    summarise_all(function(x) sum(is.na(x))) %>%
    pivot_longer(cols = everything())
)

#------------------------------------------------------------------------------#
# Summarise homes by ever affected

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
  pivot_wider(names_from = ch_type, 
              values_from = n_resid, 
              names_prefix = "Type:") %>%
  rowwise() %>%
  mutate(n_resid = sum(c_across(cols = -ever_affected))) %>% 
  ungroup() %>%
  mutate_at(vars(-n_resid, -ever_affected), 
            function(x) paste0(x, " (", round(x/.$n_resid,2), ")")) %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(-n_resid) -> tab_type

# Summarise other variables
chars_waffect %>%
  group_by(ever_affected) %>%
  summarise(N = n(),
            `No. TPP residents` = sum(n_resid, na.rm = T),
            ch_size_med = round(median(ch_size, na.rm = T),2),
            ch_size_quants = paste(round(quantile(ch_size, 
                                                  probs = c(0.25, 0.75), 
                                                  na.rm = T)), collapse = ", "),
            `N rural` = sum(rural_urban == "rural", na.rm = T),
            `% rural` = round(sum(rural_urban == "rural", na.rm = T)/N, 2),
            imd_med = round(median(imd, na.rm = T)),
            imd_quants = paste(round(quantile(imd, 
                                              probs = c(0.25, 0.75), 
                                              na.rm = T)), collapse = ", "),
            `N dementia` = sum(hh_maj_dem),
            `% dementia` = round(sum(hh_maj_dem)/N, 2)
            ) %>%
  mutate(N_perc = round(N/N_ch_tot,2),
         `N (%)` = paste0(N, " (",N_perc,")"),
         `Size, med[IQR]` = paste0(ch_size_med, " [",ch_size_quants,"]"),
         `IMD, med[IQR]` = paste0(imd_med, " [",imd_quants,"]"),
         `Rural, N (%)` = paste0(`N rural`, " (",`% rural`,")"),
         `Dementia > 50%, N (%)` = paste0(`N dementia`, 
                                          " (",`% dementia`,")"),) %>% 
  ungroup() %>% 
  remove_rownames() %>%
  column_to_rownames(var = "ever_affected") %>%
  dplyr::select(N, `No. TPP residents`, 
                `Size, med[IQR]`,
                `IMD, med[IQR]`, 
                `Rural, N (%)`,
                `Dementia > 50%, N (%)`) %>% 
  cbind(tab_type) -> tab1

print("Summarise carehome characteristics by ever affected:")
t(tab1) 

write.csv(tab1, "./ch_chars_tab.csv")

#------------------------------------------------------------------------------#
# Resident characteristics by ever affected

# Age, dementia status and ethnicity of care home residents, stratified by 
# whether or not their home was affected (percentages out of total residents in 
# that stratum):

ch_resid_all <- ch %>%
  mutate(ever_affected = "Overall")

ch %>%
  right_join(dplyr::select(ch_chars, household_id, msoa, ever_affected)) %>% 
  mutate(ever_affected = ifelse(ever_affected, "Affected", "Unaffected")) %>%
  bind_rows(ch_resid_all) %>% 
  mutate(ever_affected = factor(ever_affected, 
                                levels = c("Overall",
                                           "Affected",
                                           "Unaffected"))) -> ch_resid_all

ch_resid_all %>%
  group_by(ever_affected) %>%
  summarise(`No. TPP residents` = n(),
            med_age = round(median(age, na.rm = T)),
            q_age = paste(round(quantile(age, 
                                         probs = c(0.25, 0.75), 
                                         na.rm = T)), 
                          collapse = ", "),
            n_minor = sum(ethnicity != 1, na.rm = T),
            prop_minor = mean(ethnicity != 1, na.rm = T),
            n_dem = sum(dementia, na.rm = T)
  ) %>% 
  mutate(`age med[IQR]` = paste0(med_age, " [",q_age,"]"),
         `minority ethnicity n(%)` = paste0(n_minor, 
                                            " (",
                                            round(n_minor/`No. TPP residents`,4),
                                            ")"),
         `dementia n(%)` = paste0(n_dem, 
                                  " (",
                                  round(n_dem/`No. TPP residents`,4),
                                  ")")) %>%
  ungroup() %>%
  remove_rownames() %>% 
  column_to_rownames("ever_affected") %>%
  dplyr::select(`No. TPP residents`, 
                `age med[IQR]`, 
                `minority ethnicity n(%)`, 
                `dementia n(%)` ) -> tab_age

ch_resid_all %>%
  group_by(ever_affected, ethnicity) %>%
  summarise(n_resid = n()) %>%
  ungroup() %>%
  mutate(ethnicity = replace_na(as.character(ethnicity), "missing")) %>%
  pivot_wider(names_from = ethnicity, 
              values_from = n_resid, 
              names_prefix = "Ethn:") %>%
  rowwise() %>%
  mutate(n_resid = sum(c_across(cols = -ever_affected))) %>% 
  ungroup() %>%
  mutate_at(vars(-n_resid, -ever_affected), 
            function(x) paste0(x, 
                               " (", 
                               round(x/.$n_resid,4), 
                               ")")) %>%
  column_to_rownames("ever_affected") %>%
  dplyr::select(-n_resid) -> tab_ethn

tab2 <- cbind(tab_age, tab_ethn)

print("Summarise resident characteristics by ever affected:")
t(tab2)

#------------------------------------------------------------------------------#

#----------------------#
#       FIGURES        #
#----------------------#

# pdf(file = "./descriptive.pdf", height = 7, width = 9)

# Age distribution
ggplot(input, aes(age)) +
  geom_histogram() +
  facet_wrap(~ care_home_type, scales = "free") -> age_hist
ggsave("./age_dist.png", age_hist, height = 5, width = 6, units = "in")


# Care home survival
ch_long %>%
  group_by(date) %>%
  filter(first_event > date) %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(title = "Survival of care homes from COVID-19 introduction",
       x = "", y = "No. without event") -> surv1
ggsave("./ch_survival.png", surv1, height = 5, width = 6, units = "in")

ch_long %>%
  group_by(date, ch_type) %>%
  filter(first_event > date & ch_type != "PS") %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n, col = ch_type)) +
  geom_line() +
  labs(title = "Survival of care homes from COVID-19 introduction",
       x = "", y = "No. without event", col = "Type") -> surv2
ggsave("./ch_survival_bytype.png", surv2, height = 5, width = 6, units = "in")


# Type of first event
ch_chars %>%
  filter(ever_affected) %>% 
  group_by(household_id) %>%
  summarise(first_event = unique(first_event),
            first_event_which = unique(first_event_which)) %>%
  mutate(first_event_which = factor(first_event_which, levels = event_dates, 
                                    labels = c("Primary care probable diagnosis", 
                                               "Positive test result", 
                                               "Hospital admission (confirmed/suspected)", 
                                               "Death (confirmed/suspected)"))) %>%
  ggplot(aes(first_event, fill = first_event_which)) +
  geom_histogram() + 
  theme_minimal() +
  theme(legend.position = c(0.8,0.8)) +
  labs(x = "Date of first COVID event", y = "Frequency", fill = "") -> first_events
ggsave("./first_event_type.png", first_events, height = 5, width = 7, units = "in")

#------------------------------------------------------------------------------#

## Community burden
# Average daily incidence
ch_long %>%
  group_by(date) %>%
  summarise(msoa_mean = mean(msoa_roll7, na.rm = T),
            eng_roll7 = unique(eng_roll7, na.rm = T)) %>%
  ungroup() -> comm_inc_avg

# Community incidence over time
ch_long %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = msoa_roll7, group = msoa), col = "grey", alpha = 0.05) +
  geom_line(data = comm_inc_avg, aes(y = msoa_mean), col = "white", lty = "dashed", lwd = 1.5) + 
  geom_line(data = comm_inc_avg, aes(y = eng_roll7), col = "steelblue", lty = "dashed", lwd = 1.5) + 
  labs(title = "Rolling seven day incidence per 100,000",
       subtitle = "Probable cases per MSOA (grey/white) and confirmed cases nationally (blue)",
       x = "", y = "Rate") +
  scale_x_date(limits = study_per) -> comm_inc_time
ggsave("./community_inc.png", comm_inc_time, height = 5, width = 7, units = "in")

#------------------------------------------------------------------------------#

## Community incidence versus care home introduction

dat %>%
  mutate(event_ahead = as.factor(event_ahead)) %>%
  pivot_longer(c("msoa_roll7","msoa_lag1wk","msoa_lag2wk","eng_roll7","eng_lag1wk","eng_lag2wk")) %>% 
  ggplot(aes(value, event_ahead)) +
  geom_boxplot() +
  labs(title = "Community incidence versus 14-day-ahead introduction",
       x = "Daily cases in community, per 100,000",
       y = "Introduction in next 14 days") +
  facet_grid(rows = "name", scales = "free") -> comm_v_ch
ggsave("./comm_vs_ch_risk.png", comm_v_ch, height = 8, width = 10, units = "in")

# Log2 scale
dat %>%
  mutate(event_ahead = as.factor(event_ahead)) %>%
  pivot_longer(c("msoa_roll7","msoa_lag1wk","msoa_lag2wk","eng_roll7","eng_lag1wk","eng_lag2wk")) %>% 
  # Avoid zeros for log transform
  mutate(value = value + mean(value, na.rm = T)/100) %>%
  ggplot(aes(value, event_ahead)) +
  geom_boxplot() +
  labs(title = "Community incidence versus 14-day-ahead introduction",
       x = "Daily cases in community, per 100,000",
       y = "Introduction in next 14 days") +
  facet_grid(rows = "name", scales = "free") +
  scale_x_continuous(trans = "log2") -> comm_v_ch_log2
ggsave("./comm_vs_ch_risk_log2.png", comm_v_ch_log2, height = 8, width = 10, units = "in")


#------------------------------------------------------------------------------#

## Community, care home and older population epidemics
## Currently just absolute numbers as don't have denominator of population in 
## community and care home per MSOA

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
  theme(legend.position = c(0.2,0.8)) -> comp_epi
ggsave("./compare_epidemics.png", comp_epi, height = 5, width = 7, units = "in")

# dev.off()

################################################################################

time_desc <- Sys.time() - time_desc
write(paste0("Total time running descriptive: ",round(time_desc,2)), file="log_descriptive.txt", append = TRUE)

sink() 

################################################################################
