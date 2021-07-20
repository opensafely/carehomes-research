################################################################################
# Description: Check uniqueness of household ID
#
################################################################################

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(data.table)

sink("./check_hhid.txt", type = "output")

theme_set(theme_minimal())

options(datatable.old.fread.datetime.character = TRUE)

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# args <- c("input.csv")
args = commandArgs(trailingOnly = TRUE)

dat <- fread(args[1], data.table = FALSE, na.strings = "") 

# Redefine missing value codes in IMD and rural index
dat$imd <- na_if(na_if(dat$imd, -1),0)
dat$rural_urban <- na_if(dat$rural_urban, -1)

#--------------------------#
#      COMPLETENESS        #
#--------------------------#

# Household ID

print("No. with missing household id:")
summary(dat$household_id == 0)
summary(is.na(dat$household_id))

dat <- filter(dat, household_id > 0 & !is.na(household_id))


# TPP coverage

print("No. with missing/0 TPP coverage:")
summary(dat$percent_tpp == 0)
summary(is.na(dat$percent_tpp))

ggplot(dat, aes(percent_tpp)) +
  geom_histogram(bins = 50) -> percent_tpp_hist

ggsave("household_tpp_coverage.png",
       percent_tpp_hist,
       height = 4, width = 6, units = "in")

dat$percent_tpp <- na_if(dat$percent_tpp,0)


#-------------------------------#
#     HOUSEHOLD SIZE - TPP      #
#-------------------------------#

print("No. with missing HH size:")
summary(dat$household_size == 0)
summary(is.na(dat$household_size))

dat$household_size <- na_if(dat$household_size,0)


# By individual:
 
dat %>% 
  group_by(household_id) %>%
  mutate(household_n = n(),
         diff_size_count = household_size - household_n) -> dat

print("Individuals with discrepant household sizes:")
summary(dat$diff_size_count != 0)


# By household: 

dat %>% 
  group_by(household_id) %>%
  summarise(household_size_distinct = n_distinct(household_size, na.rm = T),
            household_size_mean = mean(household_size),
            household_n = n(),
            diff_size_count = household_size_mean - household_n) -> hh_size_check

summary(hh_size_check)

print("Households with discrepant sizes:")
summary(hh_size_check$diff_size_count != 0)


# Distribution of household size and discrepancies:

hh_size_check %>% 
  pivot_longer(-household_id, names_to = "variable") %>%
  ggplot(aes(value)) +
    geom_histogram(bins = 50) +
    facet_wrap(~variable, scales = "free")

ggsave("household_size_comparison.png",
       height = 6, width = 8)


print("Household size by care home type:")
dat %>%
  filter(!is.na(household_size)) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_size),
            sd = sd(household_size),
            median = median(household_size),
            minmax = paste(min(household_size), max(household_size), sep = ", ")) 

print("Number of records by care home type:")
dat %>%
  group_by(care_home_type, household_id) %>%
  summarise(household_n = n()) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_n),
            sd = sd(household_n),
            median = median(household_n),
            minmax = paste(min(household_n), max(household_n), sep = ", ")) 



#---------------------------------#
#     HOUSEHOLD SIZE - TOTAL      #
#---------------------------------#

# Adjusted for TPP coverage

dat %>% 
  mutate(household_size_tot = household_size/(percent_tpp/100)) -> dat



#-------------------------------------------#
#      UNIQUENESS OF CHARACTERISTICS        #
#-------------------------------------------#

print("Uniqueness of household characteristics over all residents:")
dat %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa, na.rm = T), 
            region = n_distinct(region, na.rm = T),
            household_size = n_distinct(household_size, na.rm = T),
            percent_tpp = n_distinct(percent_tpp, na.rm = T),
            care_home_type = n_distinct(care_home_type, na.rm = T),
            imd = n_distinct(imd, na.rm = T),
            rural_urban = n_distinct(rural_urban, na.rm = T)) -> n_distinct_chars

# Should be one distinct value for every household
summary(n_distinct_chars)

print("No. households with non-unique characteristics across residents:")
n_distinct_chars %>%
  dplyr::select(-household_id) %>%
  summarise(across(everything(), function(x) sum(x > 1)))

# ---------------------------------------------------------------------------- #


################################################################################

sink()

################################################################################
