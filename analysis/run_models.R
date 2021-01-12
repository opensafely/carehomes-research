################################################################################
# Description: Run all models on training data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

library(tidyverse)
library(data.table)
library(sandwich)
library(boot)
library(lmtest)

args = commandArgs(trailingOnly=TRUE)
cutoff <- args[3]
# cutoff<- 100
sink(paste0("./output_model_run_", cutoff, ".txt"), type = "output")
write("Run models",file=paste0("log_model_run_",cutoff,".txt"))

###############################################################################

dat <- readRDS(args[1])
tpp_cov <- readRDS(args[2])

# --------------------------------- Subset data ------------------------------ #

# Exclude MSOAs with less than specified cut off of TPP coverage, as estimated
# from TPP household size and MSOA populations
msoa_exclude <- tpp_cov %>%
  filter(tpp_cov < cutoff) %>%
  pull(msoa) %>%
  unique()

write(paste0("MSOAs excluded: n = ",length(msoa_exclude)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
dat <- filter(dat, !msoa %in% msoa_exclude)

# Remove rows with NA for any covariate of interest
dat_na_rm <- dat %>%
  filter_at(vars(ch_size,ch_type,hh_med_age,hh_p_female,hh_maj_ethn,hh_p_dem), all_vars(!is.na(.)))

write(paste0("Rows excluded due to missing covariates: n = ",nrow(dat)-nrow(dat_na_rm)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
dat <- dat_na_rm

# Subset time to account for 7-day time lag
dat <- dat %>%
  filter_at(vars(probable_cases_rate,probable_chg7,probable_roll7), all_vars(!is.na(.)))

# ------------------------ Split data into training and test------------------ #

samp <- sample(unique(dat$household_id),0.9*n_distinct(dat$household_id))
train <- filter(dat, household_id %in% samp)
test <- filter(dat, !household_id %in% samp)

# Save test data for use in model validation
saveRDS(test, paste0("./testdata_",cutoff,".rds"))

## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ ch_size + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem

# Time-varying (1): current day cases
f1 <- event_ahead ~ ch_size + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_cases_rate

# Time-varying (2): 7-day change
f2 <- event_ahead ~ ch_size + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_chg7

# Time-varying (3): 7-day rolling average
f3 <- event_ahead ~ ch_size + ch_type + hh_med_age + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7

# Time varying (4): Lagged
f4a <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag1wk
f4b <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag2wk

formulae <- list(base = f0, fixed = f1, week_change = f2, roll_avg = f3, roll_avg_lag1 = f4a, roll_avg_lag2 = f4b)

## --------------------------------- Fitting --------------------------------- ##

time1 <- Sys.time()
fits <- lapply(formulae, function(f) stats::glm(f, family = "binomial", data = train))
write(paste0("Time fitting models: ",round(time1-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

print("Summary: Model fits")
lapply(fits, summary)

# 10-fold cross-validation
time2 <- Sys.time()
cv_err <- lapply(formulae, function(f) boot::cv.glm(data = train, glmfit = stats::glm(f, family = "binomial", data = train), K = 10))
write(paste0("Time running cross-validation: ",round(time2-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

print("Cross-validated estimate of prediction error [raw / adj for k-fold rather than LOO]:")
err <- lapply(cv_err, function(cv) cv$delta[2])
print(err)
fit_opt <- fits[[which.min(err)]]

# Robust SEs for coefficient significance:
print_coeffs <- function(fit){
print(fit$formula)
serr <- sandwich::vcovCL(fit, cluster = train$household_id)
coeffs <- lmtest::coeftest(fit, vcov. = serr)
print(coeffs)
}

print("Summary: Model coeffs with robust SEs")
lapply(fits, print_coeffs)

print("Brier score w/ training data")
brier <- function(mod) mean(mod$residuals^2)
brier_score_train <- lapply(fits, brier)
brier_score_train

################################################################################

saveRDS(fit_opt, paste0("./fit_opt_",cutoff,".rds"))
write.csv(coeffs,paste0("./coeffs_bestmod_",cutoff,".csv"))

################################################################################

sink()

################################################################################
