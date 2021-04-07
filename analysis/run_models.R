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
library(sf)

# args <- c("analysisdata.rds", "community_prevalence.rds", "data/msoa_shp.rds", 0.1)
args <- commandArgs(trailingOnly=TRUE)

sink("./output_model_run.txt")
write("Run models",file="log_model_run.txt")

###############################################################################

dat <- readRDS(args[1])
tpp_cov <- readRDS(args[2])
msoa_shp <- readRDS(args[3])
test_sample <- as.numeric(args[4])

# --------------------------------- Subset data ------------------------------ #

# Remove rows with NA for any covariate of interest
dat_na_rm <- dat %>%
  filter_at(vars(ch_size, ch_type, imd_quint, rural_urban, hh_med_age, hh_p_female, hh_dem_gt25, hh_prop_min), all_vars(!is.na(.)))

write(paste0("Rows excluded due to missing covariates: n = ",nrow(dat)-nrow(dat_na_rm)),file="log_model_run.txt", append = T)
dat <- dat_na_rm

# Subset time to account for 7-day time lag
dat <- dat %>%
  filter_at(vars(probable_cases_rate,probable_roll7,probable_roll7_lag2wk), all_vars(!is.na(.)))

print("Summary: NA filtered data")
summary(dat)

print("No. care homes in final analysis data:")
n_distinct(dat$household_id)

# Add 1 to probable cases to use log transform
dat %>%
  mutate(across(c(probable_cases_rate, probable_cases_rate_nb, probable_roll7:probable_roll7_nb_lag2wk), function(x) log((x+1),base = 2), .names = "log2_{.col}")) -> dat

# ------------------------ Split data into training and test------------------ #

samp <- sample(unique(dat$household_id),(1-test_sample)*n_distinct(dat$household_id))
train <- filter(dat, household_id %in% samp)
test <- filter(dat, !household_id %in% samp)

# Save test data for use in model validation
saveRDS(test,"./testdata.rds")

print("No. care homes in training data:")
train %>% 
  group_by(ever_affected) %>%
  summarise(n_ch = n_distinct(household_id))

print("No. care homes in testing data:")
test %>% 
  group_by(ever_affected, wave) %>%
  summarise(n_ch = n_distinct(household_id))

## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave

# Time-varying (1): current day cases
f1a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_cases_rate
f1b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_cases_rate_nb

# Time-varying (2): 7-day rolling average
f2a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_roll7
f2b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_roll7_nb

# Time varying (3): Lagged
f3a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_roll7_lag1wk + log2_probable_roll7_lag2wk
f3b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + wave + log2_probable_roll7_nb_lag1wk + log2_probable_roll7_nb_lag2wk

# # Time interaction (4)
f4a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + log2_probable_roll7*wave
f4b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_med_age + hh_p_female + hh_dem_gt25 + hh_prop_min + log2_probable_roll7_nb*wave

formulae <- list(base = f0, fixed = f1a, fixed_nb = f1b, roll_avg = f2a, roll_avg_nb = f2b, 
                 lagged = f3a, lagged_nb = f3b,
                 interaction = f4a, interaction_nb = f4b)


## ------------------------- Check variable levels ---------------------------##

print("Summary: Training data")
summary(train)

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
write(paste0("Time fitting models: ",round(time1-Sys.time(),2)), file="log_model_run.txt", append = TRUE)

print("Summary: Model fits")
lapply(fits, summary)


# ypred<-predict(fits[[7]], newdata = train, type = "response")
# plot(train$day,train$event_ahead)
# points(train$day,ypred,col="blue")

# Robust SEs for coefficient significance:
print_coeffs <- function(fit){
  confints <- coefci(fit, df = Inf, vcov = vcovCL, cluster = train$household_id)
  testcoeffs <- lmtest::coeftest(fit, vcov = vcovCL, cluster = train$household_id)
  
  out <- as.data.frame(round(cbind(exp(cbind(testcoeffs[,1],confints)),testcoeffs[,4]),4))
  names(out) <- c("Estimate","2.5%","97.5%","Pr(>|z|)")
  
  # print(fit$formula)
  return(out)
}

print("Summary: Model coeffs with robust SEs")
lapply(fits, print_coeffs)

print("Model comparison on training data:")

brier_train <- function(fit) mean((fit$fitted.values - train$event_ahead)^2)

data.frame(AIC = sapply(fits, AIC),
           Brier = sapply(fits, brier_train),
           cv_err = sapply(formulae, function(f) boot::cv.glm(data = train, glmfit = stats::glm(f, family = "binomial", data = train), K = 10)$delta[2])) %>% 
  rownames_to_column(var = "Model") %>%
  mutate(diffAIC = AIC - min(AIC),
         diffBrier = Brier - min(Brier),
         diffCV = cv_err - min(cv_err)) %>%
  arrange(diffAIC) %>%
  mutate(across(-Model, function(x) round(x,6))) -> model_comp

model_comp

## MAP RESIDUALS ##

# map_resids <- function(fit){
# 
# train %>%
#   mutate(res = residuals(fit, type = "pearson")) %>%
#   group_by(msoa) %>%
#   summarise(mean_res = mean(res, na.rm = TRUE)) -> msoa_resids
#   
#   try(
#     msoa_shp %>% 
#       full_join(msoa_resids, by = c("MSOA11CD" = "msoa")) %>%
#       ggplot(aes(geometry = geometry, fill = mean_res)) +
#       geom_sf(lwd = 0) +
#       scale_fill_viridis_c() +
#       theme_minimal() -> map, silent = TRUE)
#   
#   return(map)
# 
# 
# }

# pdf("model_resids_map.pdf", height = 10, width = 8)
# lapply(fits, map_resids)
# dev.off()

################################################################################

saveRDS(fits, "./fits.rds")

################################################################################

sink()

################################################################################
