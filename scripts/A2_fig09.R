## ===================================
## Additional file 2 Fig 09
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_SeasonalityTiming_extended"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))

### load information from Figure 2 dat
load(file.path("dat", "SeasonalDat.Rdata"))
seasons <- c("MidOfRain", "DrySeason", "BeginofRain", "EndofRain", "ThroughoutTheYear")


rawdat_combined_cum$deploy_mth_adj <- factor(rawdat_combined_cum$deploy_mth,
  levels = monthAdjustDat$month,
  labels = monthAdjustDat$month_adj
)
table(rawdat_combined_cum$deploy_mth_adj, rawdat_combined_cum$deploy_mth)
rawdat_combined_cum$deploy_mth_adj <- as.numeric(as.character(rawdat_combined_cum$deploy_mth_adj))


## redefine seasonality
rawdat_combined_cum$MidOfRain <- 0
rawdat_combined_cum$DrySeason <- 0
rawdat_combined_cum$BeginofRain <- 0
rawdat_combined_cum$EndofRain <- 0
rawdat_combined_cum$ThroughoutTheYear <- 0

rawdat_combined_cum$MidOfRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(-3)] <- 1
rawdat_combined_cum$DrySeason[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(3)] <- 1
rawdat_combined_cum$BeginofRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(-5)] <- 1
rawdat_combined_cum$EndofRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(0)] <- 1
rawdat_combined_cum$ThroughoutTheYear[rawdat_combined_cum$Duration_mth == 12] <- 1

## Define variables
timeVARS <- c(
  "timestep", "MonitoringDays", "PostInterventionDates_1", "PostInterventionDates_2", "PostInterventionDates_3",
  "year", "deploy_timestep_2002", "deploy_timestep_2003", "deploy_dates_2002", "deploy_dates_2003"
)
groupVARS <- c("seed", "EIR", "SeasonID", "SeasonID_label", "Duration_mth", "deploy_mth", "Duration_yr", "developmentDuration", "femaleEggsLaidByOviposit", "developmentSurvival")
groupVARS_time <- c(groupVARS, timeVARS)
groupVARSALL <- c(groupVARS, "LARVcov")
groupVARSALL_time <- c(groupVARS_time, "LARVcov")


table(rawdat_combined_cum$MidOfRain)
tempgroupvars <- groupVARSALL[!(groupVARSALL %in% c("deploy_mth", "Duration_mth"))]


## function to filter and aggregate dataset
tempFun <- function(filterVar, tempgroupVARS = tempgroupvars) {
  # filterVar="ThroughoutTheYear"
  # filterVar="MidOfRain"

  if (filterVar == "MidOfRain") name <- "Mid of rain"
  if (filterVar == "BeginofRain") name <- "Begin of rain"
  if (filterVar == "EndofRain") name <- "End of rain"
  if (filterVar == "DrySeason") name <- "Dry season"
  if (filterVar == "ThroughoutTheYear") name <- "Throughout the year"

  tempdat <- rawdat_combined_cum
  tempdat$filterVar <- tempdat[, colnames(tempdat) == filterVar]


  testing <- FALSE
  if (testing) {
    ggplot(data = subset(rawdat_combined_cum, LARVcov == 80 & EIR == 10 & seed == 1 & ThroughoutTheYear == 1 & PostInterventionTimesteps_1 >= -10 & PostInterventionTimesteps_1 <= 100)) +
      annotate("rect", xmin = 0, xmax = 73, ymin = -Inf, ymax = Inf, fill = "deepskyblue2", alpha = 0.3) +
      geom_line(data = subset(rawdat_combined_cum, LARVcov == 0 & EIR == 10 & seed == 1 & ThroughoutTheYear == 1 & PostInterventionTimesteps_1 >= -10 & PostInterventionTimesteps_1 <= 100), aes(x = PostInterventionTimesteps_1, y = scale(Nv0))) +
      geom_line(aes(x = PostInterventionTimesteps_1, y = scale(red_PR))) +
      facet_wrap(EIR ~ SeasonID, nrow = 3)

    ggplot() +
      geom_line(data = subset(temp_yr1, seed == 1), aes(x = LARVcov, y = red_PR, col = Seasonality_grp)) +
      geom_line(data = subset(temp_yr12, seed == 1), aes(x = LARVcov, y = red_PR, col = Seasonality_grp)) +
      facet_wrap(EIR ~ SeasonID, nrow = 3)
  }

  ## corrected 10.07.2019 ==73 and not <=73
  temp_yr1 <- tempdat %>%
    filter(PostInterventionTimesteps_1 == 73) %>%
    filter(filterVar == 1) %>%
    group_by_(.dots = tempgroupVARS) %>%
    summarize(
      red_simulated.EIR = mean(red_simulated.EIR),
      red_PR = mean(red_PR),
      red_Nv0 = mean(red_Nv0),
      red_Nv = mean(red_Nv),
      red_simulated.EIR_cum = cummean(red_simulated.EIR),
      red_PR_cum = cummean(red_PR),
      red_Nv0_cum = cummean(red_Nv0),
      red_Nv_cum = cummean(red_Nv)
    ) %>%
    group_by() %>%
    mutate(Duration_yr = 1, Seasonality_grp = name)


  temp_yr2 <- tempdat %>%
    filter(PostInterventionTimesteps_2 == 73) %>%
    filter(filterVar == 1) %>%
    group_by_(.dots = tempgroupVARS) %>%
    summarize(
      red_simulated.EIR = mean(red_simulated.EIR),
      red_PR = mean(red_PR),
      red_Nv0 = mean(red_Nv0),
      red_Nv = mean(red_Nv),
      red_simulated.EIR_cum = cummean(red_simulated.EIR),
      red_PR_cum = cummean(red_PR),
      red_Nv0_cum = cummean(red_Nv0),
      red_Nv_cum = cummean(red_Nv)
    ) %>%
    group_by() %>%
    mutate(Duration_yr = 2, Seasonality_grp = name)

  tempdat <- rbind(temp_yr1, temp_yr2)

  return(tempdat)
}


seasons <- c("MidOfRain", "DrySeason", "BeginofRain", "EndofRain", "ThroughoutTheYear")


## apply function to all seasons
datList <- list()
count <- 0
for (season in seasons) {
  count <- count + 1
  datList[[count]] <- tempFun(season)
}
rawdat_combined_AggrSeason <- do.call("rbind", datList)
rm(datList)


rawdat_combined_AggrSeason$EIRlabel <- paste0("EIR ", rawdat_combined_AggrSeason$EIR)
rawdat_combined_AggrSeason$EIRlabel <- factor(rawdat_combined_AggrSeason$EIRlabel,
  levels = c("EIR 3", "EIR 10", "EIR 90"),
  labels = c("EIR 3", "EIR 10", "EIR 90")
)

table(rawdat_combined_AggrSeason$SeasonID_label)

### aggregated seeds
plotdatAggrSeeds <- rawdat_combined_AggrSeason %>%
  filter(Duration_yr == 1) %>%
  dplyr::group_by(EIR, EIRlabel, SeasonID, Duration_yr, SeasonID_label, Seasonality_grp, LARVcov) %>%
  dplyr::summarize(red_PR = mean(red_PR))



plotdat <- subset(rawdat_combined_AggrSeason, SeasonID %in% c("0", "1-1", "2-1", "2-2") & Duration_yr == 1)


pplot <- ggplot(data = plotdat) +
  theme_cowplot() +
  annotate("rect", xmin = 60, xmax = 100, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_smooth(aes(x = LARVcov, y = red_PR, col = Seasonality_grp, fill = Seasonality_grp), span = 0.9) +
  labs(
    x = "Larviciding coverage (%)", y = "Relative reduction in prevalence (%)\n[t=365]",
    color = "Seasonal deployment", fill = "Seasonal deployment"
  ) +
  facet_grid(EIRlabel ~ SeasonID_label) +
  customTheme_noAngle +
  scale_y_continuous(expand = c(0, 0), lim = c(-10, 100)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = c(-Inf, 0, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  scale_color_manual(values = FiveCols) +
  scale_fill_manual(values = FiveCols) +
  geom_abline(intercept = 0, slope = 1)



ggsave("A2_Fig9.png", plot = pplot, path = file.path("additional_figures"), width = 15, height = 8, device = "png")
# ggsave("A2_Fig9.pdf", plot = pplot, path = file.path("additional_figures"), width = 17, height = 7, device = "pdf")

fwrite(plotdat, file = file.path("additional_figures", "figuredat", "A2_Fig09.csv"), row.names = FALSE)
