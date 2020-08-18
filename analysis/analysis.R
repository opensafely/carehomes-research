################################################################################
# Description: Primary analysis script for "Spatiotemporal risk of 
# infection of care homes during the first wave of the COVID-19 pandemic in the 
# UK"
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

source("./analysis/data_setup.R")

# Split data into training and test, and drop any rows with missing predictor values

train <- filter(lmk_data, household_id %in% samp) %>% drop_na()
test <- filter(lmk_data, !household_id %in% samp) %>% drop_na() 

# Define model formulae
f1 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + probable_cases 
f2 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + comm_probable_chg7
f3 <- event_ahead ~ ch_size + hh_p_female + hh_maj_ethn + hh_p_dem + comm_probable_roll7
#+ comm_probable_lag1 + comm_probable_lag2 + comm_probable_lag3 + comm_probable_lag4 + comm_probable_lag5 + comm_probable_lag6 + comm_probable_lag7 

### Basic model
# Need robust SEs since each care home appears multiple times in the data

f <- f1
fit <- glm(f, family = "binomial", data = train)
summary(fit)

# Robust SEs
serr <- vcovCL(fit, cluster = train$household_id)
lmtest::coeftest(fit, vcov. = serr)

### Predictions
test$pred <- predict(fit, newdata = test, type = "response")

png(filename = "./output/figures/risk_histogram.png", height = 700, width = 1000)
ggplot(test, aes(x = pred, fill = as.factor(event_ahead))) +
  geom_histogram() +
  labs(fill = "Event 14 days", x = "Predicted risk",y = "") + 
  theme_minimal()
dev.off()

verification::rps(test$event_ahead, cbind(test$pred,1-test$pred))
