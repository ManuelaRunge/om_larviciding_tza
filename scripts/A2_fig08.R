## ===================================
## Additional file 2 Fig 08
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_SeasonalityTiming_extended"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))

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


## with lines indicating the seasons
LabelDat$valueadj <- rescale(LabelDat$value, c(100, 140), c(0.07272727, 0.10000000))

table(datsubAggr$deploy_mth_adj)
table(LabelDat$month_adj)
table(SeasonalDat_long$month_adj)
SeasonalDat_long$deploy_mth_adj <- as.numeric(SeasonalDat_long$month_adj)

datsubAggrAdj <- left_join(datsubAggr, SeasonalDat_long[, c("value", "deploy_mth_adj", "season_corrected")])
datsubAggrAdj$valueadj <- rescale(datsubAggrAdj$value, c(100, 140), c(0.07272727, 0.10000000))

### plot varying coverage per season and per EIR
pplot <- ggplot(data = subset(datsubAggrAdj, SeasonID != "0" & season_corrected != "Throughout the year" & !is.na(LARVcov))) +
  geom_line(aes(x = deploy_mth_adj, y = mean.val, group = interaction(EIR, LARVcov), linetype = as.factor(LARVcov))) +
  labs(y = outcomeLabel, x = "Months relative to transmission peak", linetype = "Coverage (%)", col = "Deployment") +
  facet_wrap(EIR ~ SeasonID_label, ncol = 4, labeller = labeller(EIR = label_both, SeasonID_label = label_value)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold"),
    strip.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(limits = c(-10, 130), breaks = seq(0, 100, 25), labels = seq(0, 100, 25), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-6, 5), expand = c(0, 0), breaks = seq(-6, 5, 1), labels = seq(-6, 5, 1)) +
  geom_hline(yintercept = c(-Inf, 0, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "right") +
  scale_color_manual(values = FiveCols) +
  geom_line(aes(x = deploy_mth_adj, y = valueadj, col = season_corrected, group = season_corrected), size = 1.7)

print(pplot)
ggsave(paste0("A2_Fig08.png"), plot = pplot, path = file.path("additional_figures"), width = 12, height = 10, device = "png")
# ggsave(paste0("A2_Fig08.pdf"), plot = pplot, path = file.path("additional_figures"), width = 12, height = 10, device = "pdf")
