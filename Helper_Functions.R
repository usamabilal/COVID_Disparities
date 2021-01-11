# A few helper functions + loading of packages
# for loading data
library(tidycensus)
library(readxl)
library(foreign)
# basic packages + plotting
library(tidyverse)
library(data.table)
library(gridExtra)
library(grid)
library(lubridate)
library(scales)
library(broom)
library(zoo)
# packages with useful functions for model checking
library(MASS)
library(performance)
# mapping and spatial analysis
library(INLA)
library(spdep)
library(sf)
library(rgdal)
library(rmapshaper)
# "rescuing" a few functions (MASS is masking some of them)
select<-dplyr::select
gather<-tidyr::gather
options(scipen=999)

# function to get legends out of ggplot grobs
get_legend<-function(plot){
  grobs<-ggplotGrob(plot)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]  
  return(legend)
}
# function to get default ggplot discrete colors https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n, alpha=1) {
  hues = seq(15, 375, length = n + 1)
  t<-hcl(h = hues, l = 65, c = 100)[1:n]
  adjustcolor(t, alpha)
}