rm(list = ls())

# these are the figures in appendix a, to be ran after the estimates are ready

library(checkpoint)

checkpoint("2018-06-15", project = "own_path", checkpointLocation = "checkpoint_location")

savepath <- "savepath"
resultspath <- "resultspath" #from models_in_paper.R
wd <- "your_own_wd"

##############################
# left wing mobilization map #
##############################

# packages
# for data management
library(haven)
library(dplyr)
# for the map
library(ggplot2)
library(rworldmap)
library(RColorBrewer)
library(XLConnect)
library(viridis)
library(gridSVG)
library(grid)
library(gridExtra)
library(cowplot)
library(mapproj)

# data reading 

load(file="ess_rescaled.Rda")
myvars <- c("protest", "rideol", "year", "ideo_label", "cntry")
reg.key <- subset(data, select=c("cntry", "region"))
colnames(reg.key) <- c("country", "region")
reg.key <- reg.key[!duplicated(reg.key), ]
data <- subset(data, select=myvars)
table(data$protest, useNA = "always")
table(data$rideol, data$ideo_label, useNA = "always")
class(data$year)
data$c_id <- data$cntry
data$left[data$rideol==1 | data$rideol==2] <- 1
data$left[is.na(data$left)] <- 0
table(data$left)
data <- subset(data, select = c("c_id", "year", "left", "protest"))


length(unique(data$c_id))

# data management

head(data)
data <- data %>% 
          add_count(c_id, year)
names(data)[names(data) == 'n'] <- 'total_n'

data <- data[complete.cases(data), ]
data <- data %>% 
  group_by(ctry = as.factor(c_id), yr = as.factor(year)) %>% 
  mutate(total_pr = sum(protest)) %>%
  mutate(totleft_soc = sum(left)) %>%
  filter(protest > 0) %>% 
  mutate(totleft_prot = sum(left))

data <- subset(data, select = c("c_id", "year", "totleft_soc", "totleft_prot",
                                "total_pr", "total_n"))
data <- data %>%
          distinct # collapse to country year

data$left_in_left <- data$totleft_prot*100 / data$totleft_soc # share of left in society

data <- subset(data , select = c("c_id", "year", "left_in_left"))
data <- data %>% 
  group_by(ctry = as.factor(c_id)) %>% 
  mutate(left_in_left = mean(left_in_left))

data <- subset(data , select = c("c_id", "left_in_left"))

data <- data %>%
  distinct # collapse to country year

colnames(data)[colnames(data) == 'c_id'] <- 'country'

# View(data)

# country identifiers

data$Country <- NA
data$Country[data$country=='AT'] <- "Austria"
data$Country[data$country=='BE'] <- "Belgium"
data$Country[data$country=='BG'] <- "Bulgaria"
data$Country[data$country=='CH'] <- "Switzerland"
data$Country[data$country=='CY'] <- "Cyprus"
data$Country[data$country=='CZ'] <- "Czech Rep."
data$Country[data$country=='DE'] <- "Germany"
data$Country[data$country=='DK'] <- "Denmark"
data$Country[data$country=='ES'] <- "Spain"
data$Country[data$country=='EE'] <- "Estonia"
data$Country[data$country=='FI'] <- "Finland"
data$Country[data$country=='FR'] <- "France"
data$Country[data$country=='GB'] <- "United Kingdom"
data$Country[data$country=='GR'] <- "Greece"
data$Country[data$country=='HR'] <- "Croatia"
data$Country[data$country=='HU'] <- "Hungary"
data$Country[data$country=='IE'] <- "Ireland"
data$Country[data$country=='IS'] <- "Iceland"
data$Country[data$country=='IT'] <- "Italy"
data$Country[data$country=='LT'] <- "Lithuania"
data$Country[data$country=='LU'] <- "Luxembourg"
data$Country[data$country=='NL'] <- "Netherlands"
data$Country[data$country=='NO'] <- "Norway"
data$Country[data$country=='PL'] <- "Poland"
data$Country[data$country=='PT'] <- "Portugal"
data$Country[data$country=='SE'] <- "Sweden"
data$Country[data$country=='SI'] <- "Slovenia"
data$Country[data$country=='SK'] <- "Slovakia"

data$iso2code <- data$country
data$country <- NULL

data <- data[order(data$left_in_left),]

# map

worldMap <- getMap()

## identify EU countries

show <- which(worldMap$NAME %in% data$Country)

plotCoords <-
  lapply(seq(worldMap$NAME),
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
plotCoords <- do.call("rbind", plotCoords)

## add EU identifier
plotCoords$EU <- 0
plotCoords$EU[which(plotCoords$region %in% data$Country)] <- 1

## for some reason, this group gives a horizontal segment across Europe
plotCoords <- plotCoords[plotCoords$group != "United States4", ]


## EU coordinates
showCoords <-
  lapply(show,
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
showCoords <- do.call("rbind", showCoords)

## add left_in_left
showCoords$left_in_left <-
  as.numeric(
    gsub(",", ".", data$left_in_left[match(showCoords$region, data$Country)]))

# Figure
min(showCoords$left_in_left)
quantile(showCoords$left_in_left)
max(showCoords$left_in_left)

if(FALSE) {

protest_map <- ggplot() +
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = "white", colour = "darkgrey", size = 0.6) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group),
    fill = "grey", colour = "black", size = 0.6) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = left_in_left),
    colour = "black", size = 0.1) +
  scale_fill_gradient(
    low = "gray90", high = "gray0",
    guide=FALSE) +
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) + 
  theme_bw() +
  theme(legend.justification = c(-0.4, 1.2), legend.position = c(0, 1),
        plot.title = element_text(hjust = 0.5))

}

GeomHatch <- ggproto("GeomHatch", GeomPolygon,
                     required_aes = c("x", "y"),
                     default_aes = aes(colour = NA, fill = NA, size = 0.5, linetype = 1,
                                       alpha = NA),
                     
                     draw_panel = function(data, panel_params, coord) {
                       n <- nrow(data)
                       if (n == 1) return(zeroGrob())
                       
                       munched <- coord_munch(coord, data, panel_params)
                       # Sort by group to make sure that colors, fill, etc. come in same order
                       munched <- munched[order(munched$group), ]
                       
                       # For gpar(), there is one entry per polygon (not one entry per point).
                       # We'll pull the first value from each group, and assume all these values
                       # are the same within each group.
                       first_idx <- !duplicated(munched$group)
                       first_rows <- munched[first_idx, ]
                       
                       gl <- by(munched, munched$group, function(m){
                         
                         g <- polygonGrob(m$x, m$y, default.units = "native")
                         patternFillGrob(g, pattern = pattern(linesGrob(gp=gpar(col="black",lwd=3)),
                                                              width = unit(2, "mm"), height = unit(2, "mm"),
                                                              dev.width = 1, dev.height = 1))
                       }, simplify = FALSE)
                       gTree(children = do.call(gList, gl))
                       
                     }
)

geom_hatch <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    geom = GeomHatch, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


protest_map <- ggplot() +
  geom_hatch(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = NA, colour = NA, size = 0.6) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group),
    fill = "grey", colour = "black", size = 0.6) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = left_in_left),
    colour = "black", size = 0.1) +
  scale_fill_gradient(
    low = "gray90", high = "gray0", guide=FALSE) +
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) + 
  theme_bw() +
  theme(legend.justification = c(-0.4, 1.2), legend.position = c(0, 1))


# 
# gridsvg(name = paste0(wd, "//results//Jul_31_figures//figures//left_map.svg"),
#         res = 500, 
#         width=6, height=6)
# left_map
# dev.off(which = dev.cur())
# 
# gridsvg(name = paste0(wd, "//results//Jul_31_figures//figures//protest_map.svg"),
#         res = 500, 
#         width=6, height=6)
# protest_map
# dev.off(which = dev.cur())


data <- data[order(data$left_in_left),]
data$left_in_left <- round(data$left_in_left, 0.5)
data$bar_label <- sprintf("%.1f%%", data$left_in_left)

bar_plot <- ggplot(data, aes(x = reorder(iso2code, left_in_left), y = left_in_left)) +
  geom_bar(stat = "identity", 
           fill = "gray90",     
           width = 0.75) +
  geom_text(aes(label = bar_label),
            size = 2.5,                 
            hjust = -3) + # for some reason this number does not seem to matter
  geom_bar(stat = "identity",
           mapping = aes(y = left_in_left),
           fill = "#000000",
           width = 0.75) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(data$left_in_left) * 1.3)) +
  xlab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8))

bar_plot

comb_plot <- ggdraw() +
          draw_plot(protest_map) +
          draw_plot(bar_plot, 0.16, 0.51, 0.5, 0.44)

gridsvg(name = paste0(wd, "map_left_in_left.svg"),
                res = 500,
                width=6, height=6)
comb_plot
dev.off(which = dev.cur())


#######################
# random effect plots #
#######################

load(file=paste0(resultspath, 'm_1.RData'))

library(data.table)
library(TMB)
library(sjPlot)
library(ggplot2)
library(texreg)

screenreg(m.1[[1]])

plots <- plot_model(m.1[[1]], 
                   facet.grid = FALSE, 
                   prnt.plot = FALSE,
                   type="re")
ranef <- plots[[2]]$data

data.reg <- subset(data, select=c("cntry", "region"))
match.dat <- data.reg[!duplicated(data.reg), ]
ranef$cntry <- ranef$term
ranef <- merge(ranef, match.dat, all=T)

ranef$meanNWE <- as.numeric(0)
ranef$meanSE <- as.numeric(0)
ranef$meanEE <- as.numeric(0)

ranef$meanNWE <- mean(ranef$estimate[ranef$region==1], na.rm=T)
ranef$meanSE <- mean(ranef$estimate[ranef$region==2], na.rm=T)
ranef$meanEE <- mean(ranef$estimate[ranef$region==3], na.rm=T)

ranef$facet <- ifelse(ranef$facet == "rideol1", "Extreme Left",
                    ifelse(ranef$facet == "countryyear (Intercept)", "Intercept",
                           ifelse(ranef$facet == "rideol3", "Center",
                                  ifelse(ranef$facet == "rideol4", "Right",
                                         "Extreme Right"))))

all <- ggplot(ranef, aes(x = estimate, y = reorder(cntry, estimate))) +
  geom_point() +
  geom_segment(aes(x=conf.low, xend=conf.high, yend=cntry)) +
  xlab("Random Effect Estimate") +
  ylab("") + scale_x_continuous(breaks=c(seq(0, 3.5, 1)), limits=c(0, 3.5)) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggsave(file=paste0(savepath, "ranef_3_lvl_ctr.png"),
       plot=all,
       width = 170, height = 150, units = "mm")

load(file=paste0(resultspath, 'm_1.RData'))

library(sjPlot)
plots <- plot_model(m.1[[1]], 
                    facet.grid = FALSE, 
                    prnt.plot = FALSE,
                    type="re")
ranef <- plots[[1]]$data

library(data.table)

data.reg <- subset(data, select=c("countryyear", "region"))
match.dat <- data.reg[!duplicated(data.reg), ]
ranef$countryyear <- ranef$term
ranef <- merge(ranef, match.dat, all=T)

ranef$meanNWE <- as.numeric(0)
ranef$meanSE <- as.numeric(0)
ranef$meanEE <- as.numeric(0)

ranef$meanNWE <- mean(ranef$estimate[ranef$region==1], na.rm=T)
ranef$meanSE <- mean(ranef$estimate[ranef$region==2], na.rm=T)
ranef$meanEE <- mean(ranef$estimate[ranef$region==3], na.rm=T)

ranef$facet <- ifelse(ranef$facet == "rideol1", "Extreme Left",
                      ifelse(ranef$facet == "countryyear (Intercept)", "Intercept",
                             ifelse(ranef$facet == "rideol3", "Center",
                                    ifelse(ranef$facet == "rideol4", "Right",
                                           "Extreme Right"))))
ranef$facet <- factor(ranef$facet,
                      levels = c("Intercept", "Extreme Left", "Center",
                                 "Right", "Extreme Right"))

all <- ggplot(ranef, aes(x = estimate, y = reorder(countryyear, estimate))) +
  geom_point() +
  geom_segment(aes(x=conf.low, xend=conf.high, yend=countryyear)) +
  facet_grid(. ~ facet) + 
  xlab("Random Effect Estimate") +
  ylab("") + scale_x_continuous(breaks=c(seq(0, 20, 2)), limits=c(0, 20)) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),
                          axis.text=element_text(size=5),
                          plot.margin = unit(c(0,0,0,0), "cm"))

ggsave(file=paste0(savepath, "ranef_3_lvl.png"),
       plot=all,
       width = 170, height = 240, units = "mm")

#####################################################
# year categorical X region X distance from the gov #
#####################################################

load(file=paste0(resultspath, 'm_6.RData')) #from models_in_paper.R

low <- round(quantile(data$gov_dist)[2], digits=5)
medium <- round(quantile(data$gov_dist)[3], digits=5)
high <- round(quantile(data$gov_dist)[4], digits=5)

eff.m <- effect("region:gov_dist:year", m.6[[2]],
                xlevels=list(gov_dist=c(low, medium, high)))
eff.m <- as.data.frame(eff.m)

eff.m$region <- ifelse(eff.m$region == 1, "Northwestern Europe",
                       ifelse(eff.m$region == 2, "Southern Europe",
                              "Eastern Europe"))
eff.m$region <- factor(eff.m$region,
                       levels=c("Northwestern Europe","Southern Europe","Eastern Europe"))

eff.m$gov_dist <- ifelse(eff.m$gov_dist == low,
                         "low",
                         ifelse(eff.m$gov_dist == medium,
                                "medium", "high"))
eff.m$gov_dist <- factor(eff.m$gov_dist,
                         levels=c("low",
                                  "medium",
                                  "high"))

reg_dist_yearcat <- ggplot(eff.m, aes(year,linetype=factor(gov_dist),
                                      color = factor(gov_dist))) +
  geom_line(aes(y = fit, group=factor(gov_dist)), size=1.005) +
  geom_line(aes(y = lower,
                group=factor(gov_dist)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(gov_dist)), linetype =3) +
  facet_wrap(~ region) +
  ylab("Probability to Protest") +
  xlab("") +
  scale_colour_manual(values = c("low" = "#F40000",
                                 "medium"="#799496",
                                 "high"="#090809")) +
  scale_linetype_discrete() +
  labs(color='Distance from gov', linetype='Distance from gov') +
  theme_minimal() +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1, reverse=T),
         colour=guide_legend(keywidth = 3, keyheight = 1, reverse=T)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file= paste0(wd, "reg_dist_yearcat.png"),
       plot=reg_dist_yearcat,
       dpi=400,
       scale=0.9,
       width = 250, height = 150, units = "mm")


#################################################
# year categorical X region X personal ideology #
#################################################

load(file=paste0(resultspath, 'm_7.RData')) #from models_in_paper.R

eff.m <- effect("region:rideol:year", m.7[[1]])

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

yr_fac <- ggplot(eff.m, aes(year,linetype=factor(rideol),
                            color = factor(rideol))) +
  geom_line(aes(y = fit, group=factor(rideol)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(rideol)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(rideol)), linetype =3) +
  ylab("Probability to Protest") +
  facet_grid(. ~ region) + 
  xlab("") +
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

ggsave(file=paste0(wd, "year_dummies.png"),
       plot=yr_fac,
       scale=0.8,
       dpi=400,
       width = 250, height = 150, units = "mm")
