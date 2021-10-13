rm(list = ls())

library(checkpoint)

checkpoint("2018-06-15", project = "your_own_wd", checkpointLocation = "cp_location")

library(emmeans)

resultspath <- "resultspath" # based on models_in_paper.R


# exposure contrast

load(file=paste0(resultspath, 'm_3.RData'))

emt <- emtrends(m.3[[2]], ~ rideol | region, var = "soc_pop_eleches")
emt    # list the estimated slopes
contrast(emt, interaction = "pairwise")
# obtain interaction contrasts comparing consecutive levels of the two factors
