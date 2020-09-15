################################################################################
# Description: Preliminary data summaries
# 
# To be run following data_setup.R
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

sink("./log_descriptive.txt")

theme_set(theme_bw())

################################################################################

# Number of carehomes per MSOA
ch %>% 
  group_by(msoa) %>% 
  summarise(
    n_resid = n(),
    n_ch = n_distinct(household_id),
    n_gp = n_distinct(practice_id))

# Average number of carehomes per practice
ch %>%
  group_by(practice_id) %>%
  summarise(n_ch = n_distinct(household_id)) %>%
  ungroup() %>%
  pull(n_ch) %>%
  summary()

chars <- names(ch_chars)[-1:-2]


# comm_prev <- fread("./data/community_prevalence.csv", data.table = FALSE) %>%
#   mutate(date = ymd(date))
# lmk_data <- fread("./output/analysisdata.csv", data.table = FALSE) 

# Average daily incidence
comm_prev %>%
  group_by(date) %>%
  summarise(probable_cases_rate = mean(probable_cases_rate)) -> comm_prev_avg

pdf(file = "./output/descriptive.pdf")

# Cumulative care home survival
ch_long %>%
  group_by(date) %>%
  filter(first_event > date) %>%
  summarise(n = n_distinct(household_id)) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  labs(title = "Survival of care homes from COVID-19 introduction")

# Community incidence over time
comm_prev %>%
  ggplot(aes(date, probable_cases_rate)) +
  geom_line(aes(group = msoa), alpha = 0.1) +
  geom_line(data = comm_prev_avg, col = "white", lty = "dashed", lwd = 1.5) + 
  labs(title = "Probable cases per 100,000, by MSOA")

# Community incidence versus care home introduction
lmk_data %>%
  mutate(event_ahead = as.factor(event_ahead)) %>%
  pivot_longer(c("probable_cases_rate","probable_chg7","probable_roll7")) %>%
  ggplot(aes(event_ahead, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(rows = "name", scales = "free") +
  labs(title = "Community incidence versus 14-day-ahead introduction",
       y = "Daily probable cases in community, per 100,000",
       x = "Introduction in next 14 days")

# Discharges



dev.off()


################################################################################

sink()

################################################################################
