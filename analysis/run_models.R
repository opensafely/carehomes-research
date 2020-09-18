################################################################################
# Description: Run all models on training data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

sink(paste0("./output_model_run_", x, ".txt"))

write("Run models",file=paste0("log_model_run_",x,".txt"))

################################################################################

# --------------------------------- Subset data ------------------------------ #

# As a proxy for low TPP coverage, exclude MSOAs with less than a specified cut
# off of total probable cases per 100,000
msoa_exclude <- comm_prev %>% 
  group_by(msoa) %>%
  filter(probable_cases_rate_total < x/1e6) %>%
  pull(msoa) %>%
  unique()

# Remove rows with NA for any covariate of interest
dat <- filter(lmk_data, !msoa %in% msoa_exclude) %>%
  filter_at(vars(n_resid,rural_urban,ch_type,hh_med_age,hh_p_female,hh_maj_ethn,hh_p_dem,probable_cases_rate, probable_chg7,probable_roll7), all_vars(!is.na(.)))

# ------------------------ Split data into training and test------------------ #

samp <- sample(unique(dat$household_id),0.8*n_distinct(dat$household_id))
train <- filter(dat, household_id %in% samp)
test <- filter(dat, !household_id %in% samp)

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
write(paste0("Time fitting models: ",round(time1-Sys.time(),2)), file=paste0("log_model_run_",x,".txt"), append = TRUE)

lapply(fits, summary)

# 10-fold cross-validation
time2 <- Sys.time()
cv_err <- lapply(formulae, function(f) boot::cv.glm(data = train, glmfit = glm(f, family = "binomial", data = train), K = 10))
write(paste0("Time running cross-validation: ",round(time2-Sys.time(),2)), file=paste0("log_model_run_",x,".txt"), append = TRUE)

# Cross-validated estimate of prediction error [raw / adj for k-fold rather than LOO]:
err <- lapply(cv_err, function(cv) cv$delta[2])
print(err)
fit_opt <- fits[[which.min(err)]]

# Robust SEs for coefficient significance:
serr <- sandwich::vcovCL(fit_opt, cluster = train$household_id)
coeffs <- lmtest::coeftest(fit_opt, vcov. = serr)
print(coeffs)
write.csv(coeffs,paste0("./coeffs_bestmod_",x,".csv"))


################################################################################

sink()

################################################################################
