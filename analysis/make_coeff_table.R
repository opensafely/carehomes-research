################################################################################
# Description: Run all models on training data
#
# Author: Emily S Nightingale
# Date: 09/09/2020
#
################################################################################

library(tidyverse)

coeffs <- readRDS("coeffs_all.rds")

# Distinguish primary models from additional/supplementary, lagged models
supp <- grepl("lag", names(coeffs))

###############################################################################

cat_coeffs <- function(coeff_tab){
  coeff_tab %>%
    mutate(Estimate = paste0(round(Estimate, 2), " [", 
                             round(`2.5%`, 3), ", ",
                             round(`97.5%`, 3),"]; p = ",
                             `Pr(>|z|)`)) %>%
    dplyr::select(Coefficient, Estimate) %>%
    return()
}

coeffs_cat <- lapply(coeffs[!supp], cat_coeffs) %>%
  purrr::reduce(full_join, by = "Coefficient")
coeffs_cat_all <- lapply(coeffs, cat_coeffs) %>%
  imap(.x = ., ~ set_names(.x, c("Coefficient", .y))) %>%
  purrr::reduce(full_join, by = "Coefficient")

# Save table
write.csv(coeffs_cat, "./coeffs_table.csv", row.names = F)
write.csv(coeffs_cat_all, "./coeffs_table_all.csv", row.names = F)

################################################################################

sink()

################################################################################
