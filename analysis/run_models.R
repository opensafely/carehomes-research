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

# args <- c("analysisdata.rds", "community_prevalence.rds", "data/msoa_shp.rds", 0, 0.1)
args <- commandArgs(trailingOnly=TRUE)
msoa_shp <- readRDS(args[3])
cutoff <- as.numeric(args[4])
test_sample <- as.numeric(args[5])
sink(paste0("./output_model_run_", cutoff, ".txt"))
write("Run models",file=paste0("log_model_run_",cutoff,".txt"))

###############################################################################

dat <- readRDS(args[1])
tpp_cov <- readRDS(args[2])

# --------------------------------- Subset data ------------------------------ #

# Exclude MSOAs with less than specified cut off of TPP coverage, as estimated
# from TPP household size and MSOA populations
# msoa_exclude <- tpp_cov %>%
#   filter(tpp_cov < cutoff) %>%
#   pull(msoa) %>%
#   unique()

print("Summary: All data")
summary(dat)

dat_covcutoff <- dat %>%
  filter(tpp_cov > cutoff)

write(paste0("MSOAs excluded: n = ",n_distinct(dat$msoa)-n_distinct(dat_covcutoff$msoa)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
# dat <- filter(dat, !msoa %in% msoa_exclude)
dat <- dat_covcutoff

print("Summary: Coverage filtered data")
summary(dat)

# Remove rows with NA for any covariate of interest
dat_na_rm <- dat %>%
  filter_at(vars(ch_size, ch_type, imd_quint, rural_urban, hh_med_age, hh_p_female, hh_dem_gt25, hh_maj_ethn), all_vars(!is.na(.)))

write(paste0("Rows excluded due to missing covariates: n = ",nrow(dat)-nrow(dat_na_rm)),file=paste0("log_model_run_",cutoff,".txt"), append = T)
dat <- dat_na_rm

# Subset time to account for 7-day time lag
dat <- dat %>%
  filter_at(vars(probable_cases_rate,probable_chg7,probable_roll7,probable_roll7_lag2wk), all_vars(!is.na(.)))


print("Summary: Coverage and NA filtered data")
summary(dat)

# ------------------------ Split data into training and test------------------ #

samp <- sample(unique(dat$household_id),(1-test_sample)*n_distinct(dat$household_id))
train <- filter(dat, household_id %in% samp)
test <- filter(dat, !household_id %in% samp)

# Save test data for use in model validation
saveRDS(test, paste0("./testdata_",cutoff,".rds"))

## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn 

# Time-varying (1): current day cases
f1 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_cases_rate

# Time-varying (2): 7-day change
f2 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_chg7

# Time-varying (3): 7-day rolling average
f3 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7

# Time varying (4): Lagged
f4a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7_lag1wk
f4b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7_lag2wk

# Time interaction (5)
f5a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7:day
f5b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7_lag1wk:day
f5c <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_maj_ethn + probable_roll7_lag2wk:day


formulae <- list(base = f0, fixed = f1, week_change = f2, roll_avg = f3, roll_avg_lag1 = f4a, roll_avg_lag2 = f4b, 
                 interaction = f5a, interaction_lag1 = f5b, interaction_lag2 = f5c)

# f00 <- event_ahead ~ 1
# f0 <- event_ahead ~ ch_size
# f1 <- event_ahead ~ ch_type
# f2 <- event_ahead ~ hh_med_age
# f3 <- event_ahead ~ hh_p_female
# f4 <- event_ahead ~ hh_p_dem
# f5 <- event_ahead ~ probable_cases_rate
# f6 <- event_ahead ~ probable_roll7
# f7 <- event_ahead ~ probable_roll7_lag1wk
# f8 <- event_ahead ~ probable_roll7_lag2wk
# 
# formulae_test <- list(f00,f0,f1,f2,f3,f4,f5,f6,f7,f8)

## ------------------------- Check variable levels ---------------------------##

print("Summary: Training data")
summary(train)
# summary(test)

## --------------------------------- Fitting --------------------------------- ##

fit_mods <- function(formulae){
  
  out <- tryCatch(
    
    {
      message("Attempting model fit")
      lapply(formulae, function(f) stats::glm(f, family = "binomial", data = train))
    },
    
    error=function(cond) {
      message("Error in model fitting")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )    
  return(out) 
}


time1 <- Sys.time()
fits <- fit_mods(formulae)
write(paste0("Time fitting models: ",round(time1-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

print("Summary: Model fits")
lapply(fits, summary)

# ypred<-predict(fits[[7]], newdata = train, type = "response")
# plot(train$day,train$event_ahead)
# points(train$day,ypred,col="blue")

# Robust SEs for coefficient significance:
print_coeffs <- function(fit){
  confints <- coefci(fit, df = Inf, vcov = vcovCL, cluster = train$household_id)
  testcoeffs <- lmtest::coeftest(fit, vcov = vcovCL, cluster = train$household_id)
  
  out <- as.data.frame(round(cbind(exp(cbind(testcoeffs[,1],confints,testcoeffs[,2])),testcoeffs[,3:4]),4))
  names(out) <- c("Estimate","2.5%","97.5%","Std. Err.","z","Pr(>|z|)")
  
  # print(fit$formula)
  return(out)
}

print("Summary: Model coeffs with robust SEs")
lapply(fits, print_coeffs)

print("Brier score w/ training data")
brier <- function(mod) mean(mod$residuals^2)
brier_score_train <- lapply(fits, brier)
brier_score_train


print("10-fold cross-validation")
time2 <- Sys.time()
cv_err <- lapply(formulae, function(f) boot::cv.glm(data = train, glmfit = stats::glm(f, family = "binomial", data = train), K = 10))
write(paste0("Time running cross-validation: ",round(time2-Sys.time(),2)), file=paste0("log_model_run_",cutoff,".txt"), append = TRUE)

print("Cross-validated estimate of prediction error [raw / adj for k-fold rather than LOO]:")
err <- lapply(cv_err, function(cv) cv$delta[2])
print(err)

# fit_opt_brier <- fits[[which.min(brier_score_train)]]
# fit_opt_cv <- fits[[which.min(err)]]

## MAP RESIDUALS ##

map_resids <- function(fit){

train %>%
  mutate(res = residuals(fit, type = "pearson")) %>%
  group_by(msoa) %>%
  summarise(mean_res = mean(res, na.rm = TRUE)) -> msoa_resids
  
  if (nrow(msoa_resids) == nrow(msoa_shp)){

    msoa_shp %>% 
      full_join(msoa_resids, by = c("MSOA11CD" = "msoa")) %>%
      ggplot(aes(geometry = geometry, fill = mean_res)) +
      geom_sf(lwd = 0) +
      scale_fill_viridis_c() +
      theme_minimal() -> map
    
     return(map)
    
  }else{return("Incorrect number of observations")}

}

pdf("./model_resids_map.pdf", height = 10, width = 8)
lapply(fits, map_resids)
dev.off()

################################################################################

saveRDS(fits, paste0("./fits_",cutoff,".rds"))

################################################################################

sink()

################################################################################
