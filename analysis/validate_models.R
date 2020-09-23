################################################################################
# Description: Validate models on witheld test data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

# sink(paste0("./log_model_validate_",x,".txt"))

################################################################################

pacman::p_load("tidyverse", "lubridate")
theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)
cutoff <- args[3]

fit_opt <- readRDS(args[1])
test <- readRDS(args[2])

## ------------------------------- Prediction -------------------------------- ##

test$pred <- predict(fit_opt, newdata = test, type = "response")

# Plot histograms of predicted risk for event/no event
pdf(file = paste0("./test_pred_figs_",cutoff,".pdf"), height = 7, width = 10)

print(
  ggplot(test, aes(x = pred, fill = as.factor(event_ahead), after_stat(density))) +
  geom_histogram(binwidth = 0.01) +
  labs(fill = "Event 14 days", x = "Predicted risk",y = "Density")
)

# Boxplot of predicted risk for event/no event
print(
ggplot(test, aes(x = as.factor(event_ahead), y = pred, fill = as.factor(event_ahead))) +
  geom_boxplot() +
  labs(title = "Model-predicted risk versus observed outcome", y = "Predicted risk",x = "14-day event") +
  theme(legend.position = "none") +
  coord_flip()
)

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
print(
ggplot(roc, aes(FPR, TPR)) +
  geom_line(lty = "dashed", col = "blue") +
  geom_abline() +
  labs(title = paste0("AUC = ",auc))
)
dev.off()



################################################################################

# sink()

################################################################################
