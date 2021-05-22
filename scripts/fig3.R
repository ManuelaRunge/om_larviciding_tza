## ===================================
## FIGURE 3
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant_duration"
load(file.path("experiments", nameExperiment, "processedExpResults", "DataForAnalysis.Rdata"))

### define grouping variables for aggregation
groupVARSAggr1 <- c(
  "PostInterventionDays", "EIR", "LARVcov", "outcome", "outcomeLabel",
  "developmentSurvival", "femaleEggsLaidByOviposit", "developmentDuration"
) ### aggregate density parameter

groupVARSAggr2 <- c("PostInterventionDays", "EIR", "LARVcov", "outcome", "outcomeLabel") ### aggregate seeds

## Plot 1 - aggregate
rawdat_combined_cum_longAggr <- rawdat_combined_cum_long %>%
  dplyr::filter(EIR == 10 & LARVcov %in% c(0, 20, 40, 60, 80, 100) &
    Duration == 12 &
    PostInterventionDays >= -100 &
    PostInterventionDays <= 365 * 2 &
    cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)

### Plot 2  (different filter)
tempdat <- rawdat_combined_cum_long %>%
  dplyr::filter(Duration == 12 &
    PostInterventionDays == 365 &
    cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)

### additional filter for dataset for plotting
plotdatA <- subset(rawdat_combined_cum_longAggr, outcome != "Nv0" & PostInterventionDays <= 365)
plotdatB <- subset(tempdat, outcome != "Nv0")

### Plot 1 over time - get legend as separate object
pTimeLegend <- get_legend(ggplot() +   theme_minimal()+
  geom_line(
    data = subset(
      rawdat_combined_cum_longAggr,
      outcome != "Nv0" & LARVcov %in% c(0, 20, 40, 60, 80, 100)
    ),
    aes(x = PostInterventionDays, y = mean.val, group = LARVcov, linetype = as.factor(LARVcov)), col = "black", size = 1
  ) +
  labs(linetype = "Emergence reduction\nper deployment (%)") +
  scale_linetype_manual(
    values = c(0, 20, 40, 60, 80, 100),
    name = "Larviciding coverage (%)",
    labels = c(0, 20, 40, 60, 80, 100),
    guide = guide_legend(reverse = TRUE)
  ))

pplotLegend <- get_legend(ggplot(data = subset(tempdat, outcome != "Nv0")) + theme_minimal() +
  geom_smooth(aes(x = LARVcov, y = mean.val, linetype = as.factor(EIR)), se = FALSE, size = 1, col = "black") +
  labs(linetype = "Pre-larviciding EIR"))


pTime <- ggplot(data = plotdatA) +
  theme_cowplot() +
  geom_ribbon(aes(
    x = PostInterventionDays, ymin = min.val, ymax = max.val,
    fill = as.factor(outcomeLabel),
    linetype = as.factor(LARVcov)
  ), alpha = 0.3, show.legend = F) +
  geom_line(aes(
    x = PostInterventionDays, y = mean.val,
    col = as.factor(outcomeLabel),
    group = LARVcov,
    linetype = as.factor(LARVcov)
  ), size =1) +
  facet_grid(~outcomeLabel) +
  customTheme_noAngle +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_colour_manual(values = temp_ThreeCols) +
  labs(
    title = "A) ", x = "Days since intervention start (t)",
    y = "Relative reduction (%)",
    colour = "", fill = "",
    linetype = "Larviciding coverage (%)"
  ) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))
print(pTime)


pplot <- ggplot(data = plotdatB) +
  theme_cowplot() +
  geom_ribbon(aes(x = LARVcov, ymin = min.val, ymax = max.val, fill = as.factor(outcomeLabel), group = EIR), alpha = 0.3) +
  geom_smooth(aes(x = LARVcov, y = mean.val, col = as.factor(outcomeLabel), group = EIR, linetype = as.factor(EIR)), se=F, size = 1) +
  facet_grid(~outcomeLabel) +
  customTheme_noAngle +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_colour_manual(values =  temp_ThreeCols) +
  labs(
    title = "Predicted outcomes",
    x = "Larviciding coverage (%)",
    y = "Relative reduction (%)\n[t=365]",
    col = "", fill = "",
    linetype = "Pre-larviciding EIR"
  ) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))


## Combine plots
pIMPerfect <- pplot + labs(title = "B)")
pIMPerfect_leg <- plot_grid(pIMPerfect, pplotLegend, rel_widths = c(1, 0.3))
pTime_leg <- plot_grid(pTime, pTimeLegend, rel_widths = c(1, 0.3))
pAll <- grid.arrange(pTime_leg, pIMPerfect_leg, nrow = 2, ncol = 1)

ggsave("Figure3.png", plot = pAll, path = file.path("figures"), width = 14, height = 10, device = "png")
ggsave("Figure3.pdf", plot = pAll, path = file.path("figures"), width = 14, height = 10, device = "pdf")

rm(pIMPerfect, pIMPerfect_leg, pAll, tempdat, rawdat_combined_cum_longAggr, rawdat_combined_cum_long)

#### Save plot data
fwrite(plotdatA, file = file.path("figures", "figuredat", "Figure3_A.csv"))
fwrite(plotdatB, file = file.path("figures", "figuredat", "Figure3_B.csv"))
