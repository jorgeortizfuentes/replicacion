rm(list = ls())  # clear working directory
wd <- "own_path_to_working_directory" 
setwd(wd)  # on own computer

library(checkpoint)
checkpoint("2018-06-15", project = "your_own_wd", checkpointLocation = "cp_location")

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
  mutate(sh_pr = total_pr*100/total_n) %>% 
  filter(protest > 0) %>% 
  mutate(total_left = sum(left))

data$sh_left <- data$total_left * 100 / data$total_pr
data$sh_left_orig <- data$sh_left
data$sh_pr_orig <- data$sh_pr
before_collapse <- subset(data , select = c("c_id", "year", "sh_left_orig", "sh_pr_orig"))
data <- subset(data, select=c("c_id", "year", "sh_left", "sh_pr"))
data <- data %>% 
  group_by(c_id, year) %>% 
  summarise_all(funs(mean))

head(data)

after_collapse <- merge(data, before_collapse)
table(after_collapse$sh_left==after_collapse$sh_left_orig) # test collapse
table(after_collapse$sh_pr==after_collapse$sh_pr_orig) 

data <- data %>% 
  group_by(c_id) %>% 
  summarise_all(funs(mean))

data$year <- NULL
colnames(data)[1] <- "country"

rm("after_collapse", "before_collapse", "myvars")  # clear working directory

#regional averages

reg.key <- merge(data, reg.key, by="country")
reg.key %>%
  group_by(region) %>%
  summarise_at(vars(sh_pr), funs(mean(., na.rm=TRUE)))
reg.key <- reg.key[order(reg.key$region),]

rm("reg.key")

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

data[order(data$sh_left),]

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

## add sh_left
showCoords$sh_left <-
  as.numeric(
    gsub(",", ".", data$sh_left[match(showCoords$region, data$Country)]))

## add sh_pr
showCoords$sh_pr <-
  as.numeric(
    gsub(",", ".", data$sh_pr[match(showCoords$region, data$Country)]))

# Figure
min(showCoords$sh_left)
quantile(showCoords$sh_left)
max(showCoords$sh_left)

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
    aes(x = long, y = lat, group = group, fill = sh_pr),
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
    aes(x = long, y = lat, group = group, fill = sh_pr),
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


data <- data[order(data$sh_pr),]
data$sh_pr <- round(data$sh_pr, 0.5)
data$iso2code <- factor(data$iso2code, levels = data$iso2code)  # convert to fa

data$bar_label <- sprintf("%.1f%%", data$sh_pr)

bar_plot <- ggplot(data, aes(x = iso2code, y = sh_pr)) +
  geom_bar(stat = "identity", 
           fill = "gray90",     
           width = 0.75) +
  geom_text(aes(label = bar_label),
            size = 2.5,                 
            hjust = -3) + # for some reason this number does not seem to matter
  geom_bar(stat = "identity",
           mapping = aes(y = sh_pr),
           fill = "#000000",
           width = 0.75) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(data$sh_pr) * 1.3)) +
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
          draw_plot(bar_plot, 0.16, 0.51, 0.55, 0.48)

gridsvg(name = paste0(wd, "map_sh_protest.svg"),
                res = 500,
                width=6, height=6)
comb_plot
dev.off(which = dev.cur())


