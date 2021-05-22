##===================================
## SETTINGS
##===================================
# setwd("C:/Users/mrung/Documents/Paper/LSM paper/reproducedPipeline")

Sys.setlocale("LC_TIME", "C") ## Set language to english

#### LSM summary script

packages_needed <- c(
  "data.table", "tidyverse","iterators",
  "cowplot", "RColorBrewer", "scales",
  "reshape", "grid", "gridExtra",
  "lubridate", "zoo", "viridis"
)

lapply(packages_needed, require, character.only = TRUE)

source("rlibrary/f_grid_arrange_shared_legend.R")
source("rlibrary/f_AggrDat.R")
source("rlibrary/f_mergevars.R")
source("rlibrary/f_getDailyEIR.R")
source("rlibrary/convertDatesTimesteps_OM.R")


nHost <- 10000

showSettings=T
if(showSettings){
  
  #### Settings
  temp_ThreeCols <- c(
    "Mosquito emergence" = "grey",
    "Host-seeking mosquitoes" = "cadetblue",
    "EIR" = "orange",
    "Prevalence" = "coral2"
  )
  
  SeasonCols <- c(
    "No seasonality" = "black",
    "Low, two seasons" = "lightblue",
    "Medium, one season" = "orange",
    "Medium, two seasons" = "darkorange",
    "High, one season" = "brown",
    "High, two seasons" = "brown1"
  )
  
  cols <- c("Nv0" = "grey", "Nv" = "cadetblue", "simulated.EIR" = "orange", "PR" = "coral2", "Rainfall" = "deepskyblue2", "Incidence" = "brown")
  
  FourCols <- c(  "deepskyblue2", "darkorchid1", "orange", "palegreen4")
  
  FiveCols <- c(
    "Throughout the year" = "red2",
    "Begin of rain" = "deepskyblue2",
    "Mid of rain" = "darkorchid1",
    "End of rain" = "orange",
    "Dry season" = "palegreen4"
  )
  
  FiveCols_adj <- c(
    "Throughout the year" = "red2",
    "Begin of rain (80%)" = "deepskyblue2",
    "Wet season" = "darkorchid1",
    "End of rain (80%)" = "orange",
    "Dry season (80%)" = "palegreen4"
  )
  
  eir_cols <- c("#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04")
  prev_cols <- c("#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d")
  
  customTheme_noAngle <- theme(
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "white", fill = "white"),
    # strip.text.x = element_text(size = 16, face = "bold"),
    # strip.text.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 10),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 0),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
  
  customTheme_Angle <- theme(
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white"),
    # strip.text.x = element_text(size = 16, face = "bold"),
    # strip.text.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 10),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 90),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
  
  options(scipen = 10000)
  point <- format_format(big.mark = "'", decimal.mark = ".", scientific = FALSE)
  
}