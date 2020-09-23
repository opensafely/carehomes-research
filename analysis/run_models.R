################################################################################
# Description: Run all models on training data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

pacman::p_load("tidyverse", "lubridate", "sandwich", "boot", "lmtest")

args = commandArgs(trailingOnly=TRUE)
cutoff <- args[3]
# cutoff<- 100
sink(paste0("./output_model_run_", cutoff, ".txt"))
write("Run models",file=paste0("log_model_run_",cutoff,".txt"))

################################################################################

comm_prev <- readRDS("./community_prevalence.rds")
dat <- readRDS("./analysisdata.rds")

# --------------------------------- Subset data ------------------------------ #

# As a proxy for low TPP coverage, exclude MSOAs with less than a specified cut
# off of total probable cases per 100,000
msoa_exclude <- comm_prev %>% 
  group_by(msoa) %>%
  filter(probable_cases_rate_total < cutoff/1e5) %>%
  pull(msoa) %>%
  unique()

write(paste0("MSOAs excluded: n = ",length(msoa_exclude)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
dat <- filter(dat, !msoa %in% msoa_exclude)

# Remove rows with NA for any covariate of interest
dat_na_rm <- dat %>% 
  filter_at(vars(n_resid,rural_urban,ch_type,hh_med_age,hh_p_female,hh_maj_ethn,hh_p_dem,probable_cases_rate, probable_chg7,probable_roll7), all_vars(!is.na(.)))

write(paste0("Rows excluded due to missingness: n = ",nrow(dat)-nrow(dat_na_rm)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
dat <- dat_na_rm
      
# ------------------------ Split data into training and test------------------ #

samp <- sample(unique(dat$household_id),0.8*n_distinct(dat$household_id))
train <- filter(dat, household_id %in% samp)
test <- filter(dat, !household_id %in% samp)

# Save test data for use in model validation
saveRDS(test, paste0("./testdata_",cutoff,".rds"))

## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ n_resid + rural_urban + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem

# Time-varying (1): current day cases
f1 <- event_ahead ~ n_resid + rural_urban + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_cases_rate

# Time-varying (2): 7-day change
f2 <- event_ahead ~ n_resid + rural_urban + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_chg7

# Time-varying (3): 7-day rolling average
f3 <- event_ahead ~ n_resid + rural_urban + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7

# Time varying (4): Multiple lags
# f4a <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag1wk 
# f4b <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag2wk 

formulae <- list(base = f0, fixed = f1, week_change = f2, roll_avg = f3)

## --------------------------------- Fitting --------------------------------- ##

time1 <- Sys.time()
fits <- lapply(formulae, function(f) glm(f, family = "binomial", data = train))
write(paste0("Time fitting models: ",round(time1-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

lapply(fits, summary)

# 10-fold cross-validation
time2 <- Sys.time()
cv_err <- lapply(formulae, function(f) boot::cv.glm(data = train, glmfit = glm(f, family = "binomial", data = train), K = 10))
write(paste0("Time running cross-validation: ",round(time2-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

# Cross-validated estimate of prediction error [raw / adj for k-fold rather than LOO]:
err <- lapply(cv_err, function(cv) cv$delta[2])
print(err)
fit_opt <- fits[[which.min(err)]]

# Robust SEs for coefficient significance:
serr <- sandwich::vcovCL(fit_opt, cluster = train$household_id)
coeffs <- lmtest::coeftest(fit_opt, vcov. = serr)
print(coeffs)

################################################################################

saveRDS(fit_opt, paste0("./fit_opt_",cutoff,".rds"))
write.csv(coeffs,paste0("./coeffs_bestmod_",cutoff,".csv"))

################################################################################

sink()

################################################################################
