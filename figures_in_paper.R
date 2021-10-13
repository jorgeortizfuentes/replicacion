rm(list = ls())  # clear working directory

savepath <- "path_to_save"
type <- ".png"

resultspath <- "estimates_path" #from models_in_paper.R

# these are the figures in the paper, to be ran after the estimates are ready

library(checkpoint)
checkpoint("2018-06-15", project = "your_working_directory", checkpointLocation = "checpoint_location")

library(effects)
library(ggplot2)

#### loading files ###########

load(file=paste0(resultspath, 'm_1.RData'))
load(file=paste0(resultspath, 'm_2.RData'))
load(file=paste0(resultspath, 'm_3.RData'))
load(file=paste0(resultspath, 'm_4.RData'))
load(file=paste0(resultspath, 'm_5.RData'))
load(file=paste0(resultspath, 'm_6.RData'))

###########################################
# Figure : region X ideology interaction #
###########################################



eff.m <- effect("region:rideol", m.1[[2]])

eff.m <- as.data.frame(eff.m)

eff.m$region <- ifelse(eff.m$region == 1, "NWE",
                       ifelse(eff.m$region == 2, "SE",
                              "EE"))

eff.m$region <- factor(eff.m$region,
                       levels=c("NWE","SE","EE"))

eff.m$rideol <- ifelse(eff.m$rideol == 1, "Extreme Left",
                       ifelse(eff.m$rideol == 2, "Left",
                              ifelse(eff.m$rideol == 3, "Center",
                                     ifelse(eff.m$rideol == 4, "Right",
                                            "Extreme Right"))))
eff.m$rideol <- as.factor(eff.m$rideol)

reg_ideo <- ggplot(eff.m, aes(rideol,linetype=factor(region),
                  color = factor(region))) +
  geom_line(aes(y = fit, group=factor(region)), size=1.005) +
  geom_line(aes(y = lower,
                group=factor(region)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(region)), linetype =3) +
  ylab("Probability to Protest") +
  scale_x_discrete(name = 'Ideology of the Respondent',
                   breaks=c("Extreme Left", "Left", "Center",
                            "Right", "Extreme Right"),
                   limits=c("Extreme Left", "Left", "Center",
                            "Right", "Extreme Right")) +
  scale_colour_manual(values = c("SE" = "#F40000",
                                 "NWE"="#799496",
                                 "EE"="#090809")) +
  # scale_linetype_manual(values = c("dotted", "longdash", "solid")) +
  scale_linetype_discrete() +
  labs(color='Region', linetype='Region') +
  theme_minimal() + guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=F),
                           colour=guide_legend(keywidth = 3, keyheight = 1, reverse=F)) +
  expand_limits(y = 0)

ggsave(file= paste0(savepath, "figure1", type),
       plot=reg_ideo,
       dpi=400,
       scale=0.6,
       width = 250, height = 150, units = "mm")

# ############################################
# # Figure: Year (cont.) X Ideology X Region #
# # ##########################################

dat.key <- subset(data, select=c("year", "year_num"))
dat.key <- dat.key[!duplicated(dat.key), ]
dat.key$year_num <- round(dat.key$year_num, digits = 5)

eff.m <- effect("region:rideol:year_num", m.2[[1]],
                xlevels=list(year_num=c(dat.key$year_num)))

eff.m <- as.data.frame(eff.m)

eff.m <- merge(eff.m, dat.key, all.x = TRUE)

eff.m$region <- ifelse(eff.m$region == 1, "Northwestern Europe",
                       ifelse(eff.m$region == 2, "Southern Europe",
                              "Eastern Europe"))
eff.m$region <- factor(eff.m$region,
                       levels=c("Northwestern Europe","Southern Europe","Eastern Europe"))


eff.m$rideol <- ifelse(eff.m$rideol == 1, "Extreme Left",
                       ifelse(eff.m$rideol == 2, "Left",
                              ifelse(eff.m$rideol == 3, "Center",
                                     ifelse(eff.m$rideol == 4, "Right",
                                            "Extreme Right"))))
eff.m$rideol <- factor(eff.m$rideol,
                       levels=c("Extreme Left", "Left", "Center",
                                "Right", "Extreme Right"))

reg_ideo_yr <- ggplot(eff.m, aes(year,linetype=factor(rideol),
                                 color = factor(rideol))) +
  geom_line(aes(y = fit, group=factor(rideol)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(rideol)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(rideol)), linetype =3) +
  ylab("Probability to Protest") +
  facet_grid(. ~ region) +
  xlab("Year of the Survey") +
  ylab("Probability to Protest") +
  scale_colour_manual(values = c("Extreme Left" = "#800080",
                                 "Left" = "#EF2917",
                                 "Center"="#06AED5",
                                 "Right"="#F0C808",
                                 "Extreme Right"="chocolate4")) +
  scale_linetype_manual(values = c(2,1,3,4,5,6)) +
  labs(color='Ideology', linetype='Ideology') +
  theme_minimal() +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=T),
         colour=guide_legend(keywidth = 3, keyheight = 1, reverse=T)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  expand_limits(y = 0)

ggsave(file=paste0(savepath, "figure2", type),
       plot=reg_ideo_yr,
       dpi=400,
       width = 250, height = 150, units = "mm")

# ########################################################
# # hystorical legacy at the individual level - figure 4 #
# ########################################################

eff.m <- effect("region:rideol:soc_pop_eleches", m.3[[2]])

eff.m <- as.data.frame(eff.m)

eff.m$region <- ifelse(eff.m$region == 2, "Southern Europe",
                              "Eastern Europe")
eff.m$region <- factor(eff.m$region,
                       levels=c("Southern Europe","Eastern Europe"))

eff.m$rideol <- ifelse(eff.m$rideol == 1, "Extreme Left",
                       ifelse(eff.m$rideol == 2, "Left",
                              ifelse(eff.m$rideol == 3, "Center",
                                     ifelse(eff.m$rideol == 4, "Right",
                                            "Extreme Right"))))
eff.m$rideol <- factor(eff.m$rideol,
                       levels=c("Extreme Left", "Left", "Center",
                                "Right", "Extreme Right"))

years <- ggplot(eff.m, aes(soc_pop_eleches,linetype=factor(rideol),
                  color = factor(rideol))) +
  geom_line(aes(y = fit, group=factor(rideol)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(rideol)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(rideol)), linetype =3) +
  facet_wrap(~ region) +
  xlab("Years under the former regime") +
  ylab("Probability to Protest") +
  scale_colour_manual(values = c("Extreme Left" = "#800080",
                                 "Left" = "#EF2917",
                                 "Center"="#06AED5",
                                 "Right"="#F0C808",
                                 "Extreme Right"="chocolate4")) +
  scale_linetype_manual(values = c(2,1,3,4,5,6)) +
  labs(color='Ideology', linetype='Ideology') +
  scale_x_continuous() +
  theme_minimal() + guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=F),
                           colour=guide_legend(keywidth = 3, keyheight = 1, reverse=F))

ggsave(file=paste0(savepath, "figure3", type),
       plot=years,
       scale=0.8,
       dpi=400,
       width = 250, height = 150, units = "mm")

##############################################
### Figure 5: Party ID X Region X Ideology ###
##############################################

dat.key <- subset(data, select=c("prtdgcl", "prtdgcl_num"))
dat.key <- dat.key[!duplicated(dat.key), ]
dat.key$prtdgcl_num <- round(dat.key$prtdgcl_num, digits = 5) # error?

eff.m <- effect("region:rideol:prtdgcl_num", m.4[[2]],
                xlevels=list(prtdgcl_num=c(dat.key$prtdgcl_num)))

eff.m <- as.data.frame(eff.m)

eff.m <- merge(eff.m, dat.key, all.x = TRUE)


eff.m <- as.data.frame(eff.m)

eff.m$region <- ifelse(eff.m$region == 1, "Northwestern Europe",
                       ifelse(eff.m$region == 2, "Southern Europe",
                              "Eastern Europe"))
eff.m$region <- factor(eff.m$region,
                       levels=c("Northwestern Europe","Southern Europe","Eastern Europe"))

eff.m$rideol <- ifelse(eff.m$rideol == 1, "Extreme Left",
                       ifelse(eff.m$rideol == 2, "Left",
                              ifelse(eff.m$rideol == 3, "Center",
                                     ifelse(eff.m$rideol == 4, "Right",
                                            "Extreme Right"))))
eff.m$rideol <- factor(eff.m$rideol,
                       levels=c("Extreme Left", "Left", "Center",
                                "Right", "Extreme Right"))

eff.m$prtdgcl <- ifelse(eff.m$prtdgcl == 0, "None",
                        ifelse(eff.m$prtdgcl == 1, "Not at all close",
                               ifelse(eff.m$prtdgcl == 2, "Not close",
                                      ifelse(eff.m$prtdgcl == 3, "Quite close",
                                             "Very close"))))
eff.m$prtdgcl <- as.factor(eff.m$prtdgcl)

figure4 <- ggplot(eff.m, aes(prtdgcl,linetype=factor(rideol),
                  color = factor(rideol))) +
  geom_line(aes(y = fit, group=factor(rideol)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(rideol)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(rideol)), linetype =3) +
  ylab("Probability to Protest") +
  facet_grid(. ~ region) + 
  xlab("Partisanship of the respondent") +
  ylab("Probability to Protest") +
  scale_colour_manual(values = c("Extreme Left" = "#800080",
                                 "Left" = "#EF2917",
                                 "Center"="#06AED5",
                                 "Right"="#F0C808",
                                 "Extreme Right"="chocolate4")) +
  scale_linetype_manual(values = c(2,1,3,4,5,6)) +
  labs(color='Ideology', linetype='Ideology') + 
  theme_minimal() + 
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=F),
         colour=guide_legend(keywidth = 3, keyheight = 1, reverse=F)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file=paste0(savepath, "figure4", type),
              plot=figure4,
              scale=0.8,
              dpi=400,
              width = 250, height = 150, units = "mm")

###############################
# Figure 6: distance X region #
###############################

eff.m <- effect("region:gov_dist", m.5[[2]])
eff.m <- as.data.frame(eff.m)

eff.m$region <- ifelse(eff.m$region == 1, "NWE",
                       ifelse(eff.m$region == 2, "SE",
                              "EE"))

eff.m$region <- factor(eff.m$region,
                       levels=c("NWE","SE","EE"))

dist_region <- ggplot(eff.m, aes(gov_dist,linetype=factor(region),
                                 color = factor(region))) +
  geom_line(aes(y = fit, group=factor(region)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(region)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(region)), linetype =3) +
  ylab("Probability to Protest") +
  xlab("Distance from the government") +
  scale_colour_manual(values = c("SE" = "#F40000",
                                 "NWE"="#799496",
                                 "EE"="#090809")) +
  scale_linetype_discrete() +
  labs(color='Region', linetype='Region') +
  theme_minimal() + 
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=F),
         colour=guide_legend(keywidth = 3, keyheight = 1, reverse=F)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  expand_limits(x = 0, y = 0)

ggsave(file= paste0(savepath, "figure5", type),
       plot=dist_region,
       dpi=400,
       scale=0.9,
       width = 250, height = 150, units = "mm")

########################################################
# Figure 7: distance X gov general left-right X region #
########################################################

low <- round(quantile(data$gov_dist)[2], digits=5)
medium <- round(quantile(data$gov_dist)[3], digits=5)
high <- round(quantile(data$gov_dist)[4], digits=5)

eff.m <- effect("region:gov_genlr:gov_dist", m.6[[1]],
                xlevels=list(gov_dist=c(low, medium, high)))
eff.m <- as.data.frame(eff.m)

eff.m$gov_dist <- ifelse(eff.m$gov_dist == low,
                         "low",
                         ifelse(eff.m$gov_dist == medium,
                                "medium", "high"))
eff.m$gov_dist <- factor(eff.m$gov_dist,
                         levels=c("low",
                                  "medium",
                                  "high"))

eff.m$region <- ifelse(eff.m$region == 1, "NWE",
                       ifelse(eff.m$region == 2, "SE",
                              "EE"))

eff.m$region <- factor(eff.m$region,
                       levels=c("NWE","SE","EE"))

dist_govlr <- ggplot(eff.m, aes(gov_genlr,linetype=factor(gov_dist),
                                color = factor(gov_dist))) +
  geom_line(aes(y = fit, group=factor(gov_dist)), size=1.005) +
  geom_line(aes(y = lower,
                group=factor(gov_dist)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(gov_dist)), linetype =3) +
  facet_wrap(~ region) +
  ylab("Probability to Protest") +
  xlab("Government Left-Right position") +
  scale_colour_manual(values = c("low" = "#F40000",
                                 "medium"="#799496",
                                 "high"="#090809")) +
  scale_linetype_discrete() +
  labs(color='Distance from gov', linetype='Distance from gov') +
  theme_minimal() + guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=F),
                           colour=guide_legend(keywidth = 3, keyheight = 1, reverse=F)) +
  expand_limits(x = 0, y = 0)


ggsave(file= paste0(savepath, "figure6", type),
       plot=dist_govlr,
       dpi=400,
       scale=0.9,
       width = 250, height = 150, units = "mm")

