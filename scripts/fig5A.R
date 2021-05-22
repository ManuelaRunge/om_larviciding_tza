## ===================================
## FIGURE 5
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


table(rawdat_combined_cum$deploy_mth_adj, rawdat_combined_cum$deploy_mth, exclude = NULL)
rawdat_combined_cum$deploy_mth_adj <- as.numeric(as.character(rawdat_combined_cum$deploy_mth_adj))

## redefine seasonality
rawdat_combined_cum$MidOfRain <- 0
rawdat_combined_cum$DrySeason <- 0
rawdat_combined_cum$BeginofRain <- 0
rawdat_combined_cum$EndofRain <- 0
rawdat_combined_cum$ThroughoutTheYear <- 0

## Subset month to the resprective season
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


### Update grouping vars
# "MidOfRain","DrySeason", "BeginofRain", "EndofRain", "ThroughoutTheYear")
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

  temp_yr1 <- tempdat %>%
    dplyr::filter(PostInterventionTimesteps_1 == 73) %>%
    dplyr::filter(filterVar == 1) %>%
    dplyr::group_by_(.dots = tempgroupVARS) %>%
    dplyr::summarize(
      red_simulated.EIR = mean(red_simulated.EIR),
      red_PR = mean(red_PR),
      red_Nv0 = mean(red_Nv0),
      red_Nv = mean(red_Nv),
      red_simulated.EIR_cum = cummean(red_simulated.EIR),
      red_PR_cum = cummean(red_PR),
      red_Nv0_cum = cummean(red_Nv0),
      red_Nv_cum = cummean(red_Nv)
    ) %>%
    dplyr::group_by() %>%
    dplyr::mutate(Duration_yr = 1, Seasonality_grp = name)


  temp_yr2 <- tempdat %>%
    dplyr::filter(PostInterventionTimesteps_2 == 73) %>%
    dplyr::filter(filterVar == 1) %>%
    dplyr::group_by_(.dots = tempgroupVARS) %>%
    dplyr::summarize(
      red_simulated.EIR = mean(red_simulated.EIR),
      red_PR = mean(red_PR),
      red_Nv0 = mean(red_Nv0),
      red_Nv = mean(red_Nv),
      red_simulated.EIR_cum = cummean(red_simulated.EIR),
      red_PR_cum = cummean(red_PR),
      red_Nv0_cum = cummean(red_Nv0),
      red_Nv_cum = cummean(red_Nv)
    ) %>%
    dplyr::group_by() %>%
    dplyr::mutate(Duration_yr = 2, Seasonality_grp = name)

  tempdat <- rbind(temp_yr1, temp_yr2)

  return(tempdat)
}

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

### few seasons only
## using geom smooth for 3 seeds
plotdat <- subset(rawdat_combined_AggrSeason, SeasonID %in% c("0", "1-1", "2-1", "1-2") & Duration_yr == 1)

plotdat$Seasonality_grp2 <- factor(plotdat$Seasonality_grp,
  labels = c("Dry season", "Begin of rain", "Mid of rain", "End of rain", "Throughout the year"),
  levels = c("Dry season", "Begin of rain", "Mid of rain", "End of rain", "Throughout the year")
)

plotdatAggr <- plotdat %>%
  group_by(EIR, EIRlabel, SeasonID_label, LARVcov, Seasonality_grp2) %>%
  summarize(red_PR_mean = mean(red_PR),
            red_PR_min = min(red_PR),
            red_PR_max = max(red_PR))

pplot <- ggplot(data = plotdat) +
  theme_cowplot() +
  annotate("rect", xmin = 60, xmax = 100, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_smooth(aes(x = LARVcov, y = red_PR, col = Seasonality_grp2, fill = Seasonality_grp2), span = 0.9) +
  labs(
    x = "Larviciding coverage (%)", y = "Relative reduction\nin prevalence (%)\n[t=365]",
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
print(pplot)


pplot_sub <- ggplot(data = subset(plotdat, EIRlabel == "EIR 10" &
  SeasonID_label %in% c(
    "Medium seasonality,\none transmission season",
    "High seasonality,\none transmission season"
  ))) +
  theme_cowplot() +
  annotate("rect", xmin = 60, xmax = 100, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_smooth(aes(x = LARVcov, y = red_PR, col = Seasonality_grp2, fill = Seasonality_grp2), span = 0.9) +
  labs(
    x = "Larviciding coverage (%)", y = "Relative reduction\nin prevalence (%)\n[t=365]",
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
print(pplot_sub)


ggsave("Figure5A_full.png", plot = pplot, path = file.path("figures"), width = 16, height = 10, device = "png")
ggsave("Figure5A_full.pdf", plot = pplot, path =  file.path("figures"), width = 16, height = 10, device = "pdf")

ggsave("Figure5A_sub.png", plot = pplot_sub, path = file.path("figures"), width = 12, height = 6, device = "png")
ggsave("Figure5A_sub.pdf", plot = pplot_sub, path =  file.path("figures"), width = 12, height = 6, device = "pdf")
fwrite(plotdat, file = file.path("figures", "figuredat", paste0("Figure5_A.csv")))


pplot <- ggplot(data = subset(plotdat, EIR==90 & SeasonID==0 & Seasonality_grp2=="Dry season")) +   geom_smooth(aes(x = LARVcov, y = red_PR, col = Seasonality_grp2, fill = Seasonality_grp2), span = 0.9)
#facet_grid(EIRlabel ~ SeasonID_label)
print(pplot)
pdat <- ggplot_build(pplot)$data[[1]]
pdat %>% filter(x>=80 & x <=82)

