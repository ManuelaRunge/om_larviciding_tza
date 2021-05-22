## ===================================
## FIGURE 5B
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_SeasonalityTiming_extended"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))
load(file.path("dat", "SeasonalDat.Rdata"))

print(monthAdjustDat)

### Generate month relative to peak
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

## Subset month to the resprective season
rawdat_combined_cum$MidOfRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(-3)] <- 1
rawdat_combined_cum$DrySeason[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(3)] <- 1
rawdat_combined_cum$BeginofRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(-5)] <- 1
rawdat_combined_cum$EndofRain[rawdat_combined_cum$Duration_mth != 12 & rawdat_combined_cum$deploy_mth_adj %in% c(0)] <- 1
rawdat_combined_cum$ThroughoutTheYear[rawdat_combined_cum$Duration_mth == 12] <- 1


## aggregate seeds
groupVARS <- c("month", "EIR", "Duration_mth", "SeasonID", "SeasonID_label")
datsubAggr1 <- rawdat_combined_cum %>%
  filter(MonitoringDays >= 0 & MonitoringDays <= 365 & LARVcov == 0 & Duration_mth == 4) %>%
  dplyr::select(groupVARS) %>%
  unique() %>%
  dplyr::mutate(
    deploy_mth = month
  )

### aggregate seed per deploy_mth
groupVARS <- c("deploy_mth", "EIR", "Duration_mth", "EffectSeason", "SeasonID", "LARVcov")
outcomeMeasure <- "PR"

datsubAggr2 <- rawdat_combined_cum %>%
  filter(PostInterventionTimesteps_1 == 23 & Duration_mth == 4) %>%
  group_by_(.dots =groupVARS ) %>%
  summarize(red_PR = mean(red_PR)) 


#### Combine
datsubAggr <- datsubAggr1 %>% left_join(datsubAggr2)

datsubAggr$deploy_mth_adj <- factor(datsubAggr$deploy_mth,
                                    levels = monthAdjustDat$month,
                                    labels = monthAdjustDat$month_adj
)


table(datsubAggr$deploy_mth_adj, datsubAggr$deploy_mth)
datsubAggr$deploy_mth_adj <- as.numeric(as.character(datsubAggr$deploy_mth_adj))

datsubAggr$WetSeason <- NA
datsubAggr$WetSeason[datsubAggr$SeasonID != "0" & datsubAggr$deploy_mth_adj %in% c(-3:1)] <- -5


## add adjusted Coverage
drySeasonMths_adj <- c(-6, -5, 3, 4, 5)
betweenSeasonMths_adj <- c(-4, 2)
wetSeasonMths_adj <- c(-3, -2, -1, 0, 1)

datsubAggr$movingCoverage <- NA
datsubAggr$movingCoverage[datsubAggr$deploy_mth_adj %in% drySeasonMths_adj] <- 80
datsubAggr$movingCoverage[datsubAggr$deploy_mth_adj %in% betweenSeasonMths_adj] <- 60
datsubAggr$movingCoverage[datsubAggr$deploy_mth_adj %in% wetSeasonMths_adj] <- 40
table(datsubAggr$deploy_mth_adj, datsubAggr$movingCoverage)

plotdatMain <- subset(datsubAggr, SeasonID != "0" &
                        ((deploy_mth_adj %in% drySeasonMths_adj & LARVcov == 80) |
                           (deploy_mth_adj %in% betweenSeasonMths_adj & LARVcov == 60) |
                           (deploy_mth_adj %in% wetSeasonMths_adj & LARVcov == 40)))

#### Plot with seasonal patterns
for (eir in c(3, 10, 90)) {
  
  pplot <- ggplot(data = subset(plotdatMain,  EIR == eir & SeasonID %in% c("2-1", "1-2"))) +
    theme_cowplot() +
    geom_line(aes( x = deploy_mth_adj, y = red_PR, group = 1 ), col = "grey30", size = 1, linetype = "dashed") +
    geom_line(
      data = subset(datsubAggr, EIR == eir & LARVcov == 80 & SeasonID %in% c("2-1", "1-2")),
      aes( x = deploy_mth_adj,   y = red_PR, group = 1 ),col = "grey30", size = 1 ) +
    labs(
      y = "Relative reduction\nin prevalence (%)\n[t=120]",
      x = "Months relative to transmission peak",
      fill = "Pre-larviciding EIR",
      color = "Pre-larviciding EIR",
      caption = paste0("EIR = ", eir)
    ) +
    facet_wrap(~SeasonID_label, ncol = 2) +
    customTheme_noAngle +
    scale_y_continuous(lim = c(-5, 80), breaks = seq(0, 100, 10), labels = seq(0, 100, 10), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(-6, 5, 1), labels = seq(-6, 5, 1)) +
    geom_hline(yintercept = c(-Inf, 0, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    theme(legend.position = "none")
  
  pplot
  
  ggsave(paste0("Figure5B_EIR_", eir, ".png"), plot = pplot, path = file.path("figures"), width = 12, height = 6, device = "png")
  ggsave(paste0("Figure5B_EIR_", eir, ".pdf"), plot = pplot, path = file.path("figures"), width = 12, height = 6, device = "pdf")
  
  
  rm(pplot)
}
fwrite(plotdatMain, file = file.path("figures", "figuredat", paste0("Figure5_B_adjCov.csv")))
fwrite(datsubAggr, file = file.path("figures", "figuredat", paste0("Figure5_B_constantCov_.csv")))


#### For text
dat_adj <- plotdatMain %>% filter(EIR==10 & SeasonID %in% c("2-1", "1-2")) %>% 
  arrange(SeasonID,  red_PR) %>%
  dplyr::mutate(red_PR = round(red_PR,1)) %>%
  dplyr::select(SeasonID,  red_PR, deploy_mth_adj) %>%
  dplyr::rename(red_PR_adj = red_PR )

#### For text
dat <- datsubAggr %>%  filter(EIR==10 & SeasonID %in% c("2-1", "1-2") & LARVcov == 80) %>% 
  arrange(SeasonID,  red_PR) %>%
  dplyr::mutate(red_PR = round(red_PR,1)) %>%
  dplyr::select(SeasonID,  red_PR, deploy_mth_adj) 

dat %>% left_join(dat_adj)
