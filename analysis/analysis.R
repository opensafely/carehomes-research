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

source("./analysis/get_community_prevalence.R")

source("./analysis/data_setup.R")

source("./analysis/run_models.R")

source("./analysis/validate_models.R")


################################################################################

# sink()

################################################################################
