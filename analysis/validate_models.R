################################################################################
# Description: Validate models on witheld test data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

sink("./log_model_validate.txt")

################################################################################


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
