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

# args <- c("analysisdata.rds", "community_incidence.rds", "data/msoa_shp.rds", 0.1)
args <- commandArgs(trailingOnly=TRUE)

sink("./output_model_run.txt")
write("Run models",file="log_model_run.txt")

###############################################################################

dat <- readRDS(args[1])
comm_inc <- readRDS(args[2])
msoa_shp <- readRDS(args[3])
test_sample <- as.numeric(args[4])

# --------------------------------- Subset data ------------------------------ #

# Remove rows with NA for any covariate of interest
dat_na_rm <- dat %>%
  filter_at(vars(ch_size, ch_type, imd_quint, rural_urban, hh_dem_gt25), all_vars(!is.na(.)))

write(paste0("Rows excluded due to missing covariates: n = ",nrow(dat)-nrow(dat_na_rm)),file="log_model_run.txt", append = T)
dat <- dat_na_rm

# Subset time to account for 7-day time lag
dat <- dat %>%
  filter_at(vars(msoa_lag2wk, eng_lag2wk), all_vars(!is.na(.)))

print("Summary: NA filtered data")
summary(dat)

print("No. care homes in final analysis data:")
n_distinct(dat$household_id)

# Add 1% of mean to probable cases in order to use log transform
dat %>%
  mutate(across(c(msoa_roll7,msoa_lag1wk,msoa_lag2wk,eng_roll7,eng_lag1wk,eng_lag2wk), function(x) log((x+mean(x)/100),base = 2), .names = "log2_{.col}")) -> dat

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
f0 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25
f0a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + wave

# MSOA incidence
# 7-day rolling average 
f1 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_roll7
# Lagged
f1a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag1wk
f1b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag2wk

# Time interaction
f1c <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_roll7 + wave
f1d <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_roll7*wave

f1e <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag1wk + wave
f1f <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag1wk*wave

f1g <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag2wk + wave
f1h <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_msoa_lag2wk*wave

# National total incidence
f2 <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_roll7

# Lagged
f2a <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag1wk
f2b <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag2wk

# Time interaction
f2c <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_roll7 + wave
f2d <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_roll7*wave

f2e <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag1wk + wave
f2f <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag1wk*wave

f2g <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag2wk + wave
f2h <- event_ahead ~ ch_size + ch_type + imd_quint + rural_urban + hh_dem_gt25 + log2_eng_lag2wk*wave

formulae <- list(base = f0, base_wave = f0a,
                 msoa = f1, nat = f2, 
                 msoa_wave = f1c, msoa_int = f1d,
                 nat_wave = f2c, nat_int = f2d,
                 msoa_lag1 = f1a, msoa_lag2 = f1b, 
                 msoa_lag1_wave = f1e, msoa_lag1_int = f1f, 
                 msoa_lag2_wave = f1g, msoa_lag2_int = f1h, 
                 nat_lag1 = f2a, nat_lag2 = f2b,
                 nat_lag1_wave = f2e, nat_lag1_int = f2f,
                 nat_lag2_wave = f2g, nat_lag2_int = f2h)

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


# Robust SEs for coefficient significance:
print_coeffs <- function(fit){
  confints <- coefci(fit, df = Inf, vcov = vcovCL, cluster = train$household_id)
  testcoeffs <- lmtest::coeftest(fit, vcov = vcovCL, cluster = train$household_id)
  
  out <- as.data.frame(round(cbind(exp(cbind(testcoeffs[,1],confints)),testcoeffs[,4]),4)) %>%
    rownames_to_column(var = "Coefficient")
  names(out)[-1] <- c("Estimate","2.5%","97.5%","Pr(>|z|)")

  return(out)
}

# Output model estimates
print("Summary: Model coeffs with robust SEs")
coeffs <- lapply(fits, print_coeffs)
coeffs 

# Compare models on AIC/Brier/CV error 
print("Model comparison:")
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

# Save table
write.csv(model_comp, "./model_comp.csv", row.names = F)

# Plot coefficients
plot_coeffs <- function(coeffs){
  
  coeffs %>%
    filter(Coefficient != "(Intercept)") %>%
    mutate(Coefficient = factor(Coefficient)) %>%
    ggplot(aes(Estimate, Coefficient, xmin = `2.5%`, xmax = `97.5%`)) +
    geom_vline(xintercept = 1, lty = "dashed", col = "grey") +
    geom_linerange() +
    geom_point(col = "steelblue") 
    theme_minimal() -> p
  
  return(p)
}

plots <- lapply(coeffs, plot_coeffs)

# Save plots for all models
pdf("./model_coeffs.pdf")
plots
dev.off()

# for (p in seq_along(plots)){
#   png(sprintf("coeffs_%s.png",names(plots)[p]), height = 1500, width = 1800, res = 300)
#   print(plots[[p]])
#   dev.off()
# }

################################################################################

saveRDS(fits, "./fits.rds")

################################################################################

sink()

################################################################################
