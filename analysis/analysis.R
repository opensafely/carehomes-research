################################################################################
# Description: Primary analysis script for "Spatiotemporal risk of
# infection of care homes during the first wave of the COVID-19 pandemic in the
# UK"
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

sink("./log.txt")

################################################################################

# setwd("~/COVID-19/carehomes-research")
source("./analysis/data_setup.R")

# Split data into training and test, and drop any rows with missing predictor values
samp <- sample(unique(ch$household_id),0.8*n_distinct(ch$household_id))
train <- filter(lmk_data, household_id %in% samp) %>% drop_na()
test <- filter(lmk_data, !household_id %in% samp) %>% drop_na()

## ----------------------------- Model Formulae -------------------------------##

# Baseline: static risk factors, no time-varying community risk
f0 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem

# Time-varying (1): current day cases
f1 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + probable_cases

# Time-varying (2): 7-day change
f2 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + comm_probable_chg7

# Time-varying (3): 7-day rolling average
f3 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + comm_probable_roll7

# Time varying (4): Multiple lags
# f4 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + comm_probable_lag1 + comm_probable_lag2 + comm_probable_lag3 + comm_probable_lag4 + comm_probable_lag5 + comm_probable_lag6 + comm_probable_lag7

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

## ------------------------------- Prediction -------------------------------- ##

test$pred <- predict(fit_opt, newdata = test, type = "response")

# Plot histograms of predicted risk for event/no event
png(filename = "./risk_histogram.png", height = 700, width = 1000)
ggplot(test, aes(x = pred, fill = as.factor(event_ahead))) +
  geom_histogram() +
  labs(fill = "Event 14 days", x = "Predicted risk",y = "") +
  theme_minimal()
dev.off()

# Plot ROC and calculate AUC as simple accuracy measure (no additional packages)
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

computeAUC <- function(pos.scores, neg.scores, n_sample=100000) {
  pos.sample <- sample(pos.scores, n_sample, replace=T)
  neg.sample <- sample(neg.scores, n_sample, replace=T)
  mean(1.0*(pos.sample > neg.sample) + 0.5*(pos.sample==neg.sample))
}

pos.scores <- test$pred[test$event_ahead == 1]
neg.scores <- test$pred[test$event_ahead == 0]
auc <- computeAUC(pos.scores, neg.scores)

roc <- simple_roc(test$event_ahead,test$pred)
png(filename = "./roc.png", height = 700, width = 700)
ggplot(roc, aes(FPR, TPR)) +
  geom_line(lty = "dashed", col = "blue") +
  geom_abline() +
  labs(title = paste0("AUC = ",auc)) +
  theme_classic()
dev.off()


################################################################################

sink()

################################################################################
