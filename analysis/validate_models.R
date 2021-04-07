################################################################################
# Description: Validate models on witheld test data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

# sink(paste0("./log_model_validate_",x,".txt"))

###############################################################################

library(tidyverse)
library(data.table)

theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)

fits <- readRDS(args[1])
test <- readRDS(args[2])

sink("./output_model_val.txt", type = "output")

print("No. care homes in testing data:")
n_distinct(test$household_id)

print("Summary: Testing data")
summary(test)

## ------------------------------- Functions -------------------------------- ##

brier_test <- function(fit){
  test$pred <- predict(fit, newdata = test, type = "response")
  test %>%
    summarise(score = mean((pred - event_ahead)^2), na.rm = T) %>%
    pull(score)
}

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

## ------------------------------ Prediction -------------------------------- ##

data.frame(score = sapply(fits, brier_test)) %>% 
  rownames_to_column(var = "Model") %>%
  mutate(diff = score - min(score, na.rm = T)) %>%
  arrange(diff) %>%
  mutate(across(-Model, function(x) round(x,6))) -> brier_comp

print("Brier scores on test data:")
brier_comp

# Plot distribution of predicted risk for event/no event
pdf(file = "./test_pred_figs.pdf", height = 7, width = 10)

for (f in seq_along(fits)){

  test$pred <- predict(fits[[f]], newdata = test, type = "response")

  # Boxplot of predicted risk for event/no event
  print(
  ggplot(test, aes(x = as.factor(event_ahead), y = pred, fill = as.factor(event_ahead))) +
    geom_boxplot() +
    labs(title = paste0(names(fits)[f], ": Model-predicted risk versus observed outcome"), y = "Predicted risk",x = "14-day event",
         subtitle = paste0("Median predictions: ",round(median(test$pred[test$event_ahead == 1], na.rm = T),4), " for event = 1 and ",round(median(test$pred[test$event_ahead == 0], na.rm = T),4), " for event = 0.")) +
    theme(legend.position = "none") +
    coord_flip()
  )
  
  # ROC
  pos.scores <- test$pred[test$event_ahead == 1]
  neg.scores <- test$pred[test$event_ahead == 0]
  auc <- computeAUC(pos.scores, neg.scores)
  
  roc <- simple_roc(test$event_ahead,test$pred)
  print(
  ggplot(roc, aes(FPR, TPR)) +
    geom_line(lty = "dashed", col = "blue") +
    geom_abline() +
    labs(title = paste0(names(fits)[f],": AUC = ",round(auc,2)))
  )
}

dev.off()


################################################################################

sink()

################################################################################
