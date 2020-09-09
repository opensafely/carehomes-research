################################################################################
# Description: Run all models on training data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

sink("./log_model_run.txt")

################################################################################


## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem

# Time-varying (1): current day cases
f1 <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_cases_rate

# Time-varying (2): 7-day change
f2 <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_chg7

# Time-varying (3): 7-day rolling average
f3 <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7

# Time varying (4): Multiple lags
# f4a <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag1wk 
# f4b <- event_ahead ~ n_resid + hh_p_female + hh_maj_ethn + hh_p_dem + probable_roll7_lag2wk 

formulae <- list(base = f0, fixed = f1, week_change = f2, roll_avg = f3)

## --------------------------------- Fitting --------------------------------- ##

fits <- lapply(formulae, function(f) glm(f, family = "binomial", data = train))
lapply(fits, summary)

# 10-fold cross-validation
cv_err <- lapply(formulae, function(f) boot::cv.glm(data = train, glmfit = glm(f, family = "binomial", data = train), K = 10))

# Cross-validated estimate of prediction error [raw / adj for k-fold rather than LOO]:
err <- lapply(cv_err, function(cv) cv$delta[2])
err
fit_opt <- fits[[which.min(err)]]

# Robust SEs for coefficient significance:
serr <- sandwich::vcovCL(fit_opt, cluster = train$household_id)
coeffs <- lmtest::coeftest(fit_opt, vcov. = serr)
print(coeffs)
write.csv(coeffs,"./coeffs.csv")


################################################################################

sink()

################################################################################
