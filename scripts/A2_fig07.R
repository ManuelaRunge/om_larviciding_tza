## ===================================
## Additional file 2 Fig 07
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


groupVARS <- c("month", "EIR", "Duration_mth", "EffectSeason", "SeasonID", "SeasonID_label")
datsubAggr1 <- rawdat_combined_cum %>%
  dplyr::filter(MonitoringDays >= 0 & MonitoringDays <= 365 & LARVcov == 0 & Duration_mth == 4) %>%
  dplyr::group_by_(.dots = groupVARS) %>%
  dplyr::summarize(Nv0 = mean(Nv0), Nv = mean(Nv), simulated.EIR = mean(simulated.EIR)) %>%
  ungroup() %>%
  mutate(
    Date = as.Date(paste0("2002-", month, "-15")),
    deploy_mth = month
  ) %>%
  dplyr::group_by(EIR) %>%
  mutate(
    Nv0_scl = rescale(Nv0, to = c(0, 100), from = range(Nv0, na.rm = TRUE)),
    Nv_scl = rescale(Nv, to = c(0, 100), from = range(Nv, na.rm = TRUE)),
    simulated.EIR_scl = rescale(simulated.EIR, to = c(0, 100), from = range(simulated.EIR, na.rm = TRUE))
  )

## aggregate seeds
groupVARS <- c(
  "deploy_mth", "PostInterventionTimesteps_1", "PostInterventionDates_1",
  "EIR", "Duration_mth", "EffectSeason", "SeasonID", "SeasonID_label", "LARVcov"
)

datsubAggr2 <- rawdat_combined_cum %>%
  filter(PostInterventionTimesteps_1 >= 0 & PostInterventionTimesteps_1 <= 73 & Duration_mth == 4) %>%
  dplyr::group_by_(.dots = ) %>%
  f_aggrDat(groupVARS, "red_PR") %>%
  mutate(Date = as.Date(paste0("2002-", deploy_mth, "-15")))

outcomeLabel <- "Relative reduction in prevalence (%)\n[t=0 - 365]"

datsubAggr <- datsubAggr1 %>% left_join(datsubAggr2)

datsubAggr$deploy_mth_adj <- factor(datsubAggr$deploy_mth,
  levels = monthAdjustDat$month,
  labels = monthAdjustDat$month_adj
)
table(datsubAggr$deploy_mth_adj, datsubAggr$deploy_mth)
datsubAggr$deploy_mth_adj <- as.numeric(as.character(datsubAggr$deploy_mth_adj))

datsubAggr$Season <- NA
datsubAggr$Season[datsubAggr$deploy_mth_adj %in% c(-3:-1)] <- "Mid of rain"
datsubAggr$Season[datsubAggr$deploy_mth_adj %in% c(3:5)] <- "Dry season"
datsubAggr$Season[datsubAggr$deploy_mth_adj %in% c(0:2)] <- "End of rain"
datsubAggr$Season[datsubAggr$deploy_mth_adj %in% c(-6:-4)] <- "Begin of rain"

table(datsubAggr$Season, datsubAggr$deploy_mth_adj, exclude = NULL)

pplot <- ggplot(data = subset(datsubAggr, EIR == 10 & SeasonID != "0" & !is.na(LARVcov))) +
  theme_cowplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_line(
    data = subset(datsubAggr, deploy_mth_adj %in% c(-5, -3, 0, 3) & EIR == 10 & SeasonID != "0" & !is.na(LARVcov)),
    aes(
      x = PostInterventionDates_1,
      y = mean.val, linetype = as.factor(LARVcov),
      col = as.factor(Season),
      group = interaction(LARVcov, Season, deploy_mth_adj)
    ), size = 1
  ) +
  labs(
    y = outcomeLabel,
    x = "Days since intervention start (t)",
    linetype = "Coverage (%)",
    col = "Deployment"
  ) +
  facet_grid(Season ~ SeasonID_label) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold"),
    strip.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(
    breaks = c(0, 60, 120, 180, 240, 300, 365),
    labels = c(0, 60, 120, 180, 240, 300, 365)
  ) +
  geom_hline(yintercept = c(-Inf, 0, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "right") +
  scale_color_manual(values = FiveCols) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
    guide = guide_legend(reverse = TRUE)
  )

ggsave(paste0("A2_Fig07.png"), plot = pplot, path = file.path("additional_figures"), width = 14, height = 10, device = "png")
# ggsave(paste0("A2_Fig07.pdf"), plot = pplot, path = file.path("additional_figures"), width = 14, height = 10, device = "pdf")
