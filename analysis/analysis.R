################################################################################
# Description: Primary analysis script for "Spatiotemporal risk of
# infection of care homes during the first wave of the COVID-19 pandemic in the
# UK"
#
# Author: Emily S Nightingale
# Date: 06/08/2020
#
################################################################################

# sink("./log.txt")

################################################################################

# setwd("~/COVID-19/carehomes-research")

source("./analysis/data_setup.R")

source("./analysis/descriptive.R")

# Run analysis on several subsets of the data, excluding MSOAs with total rate 
# of < X probable cases per 100,000 as an indicator of low TPP coverage
for (x in c(0, 10, 50, 100)){
source("./analysis/run_models.R")
source("./analysis/validate_models.R")
}

################################################################################

# sink()

################################################################################
