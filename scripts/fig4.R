## ===================================
## FIGURE 4
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant"
decay <- "Decay5daystep" 

load(file.path("experiments", nameExperiment, "processedExpResults", paste0("DataForAnalysis_", decay, ".Rdata")))


## Define grouping variables for aggregation
groupVARSAggr1 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel", "developmentSurvival", "femaleEggsLaidByOviposit", "developmentDuration") ### aggregate density parameter
groupVARSAggr2 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel") ### aggregate seeds

## exclude every 25 days (n tital applications =5) to simplify plot
rawdat_combined_cum_long <- subset(rawdat_combined_cum_long, Frequency != "5")

#### take mean of seeds, then aggregate across density dependency parameters
Figure1Dat <- rawdat_combined_cum_long %>%
  dplyr::filter(cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)

### combine data (if adding "none")
plotdatA <- subset(Figure1Dat, PostInterventionDays <= 135 & EIR == 10 & (LARVcov == 80 | Frequency == "none"))

Figure1 <- ggplot(data = plotdatA) +
  theme_cowplot() +
  geom_ribbon(aes(x = PostInterventionDays, ymin = min.val, ymax = max.val, fill = outcomeLabel), alpha = 0.3) +
  geom_line(aes(x = PostInterventionDays, y = mean.val, col = outcomeLabel), size = 1) +
  labs(
    title = "A)\n",
    y = "Relative reduction (%)",
    x = "Days since intervention start (t)",
    col = "", fill = ""
  ) +
  facet_grid(~Frequency_fct, space = "free") +
  customTheme_noAngle +
  scale_x_continuous(breaks = seq(0, 130, 30)) +
  scale_y_continuous(breaks = seq(0, 110, 25), limits = c(-5, 110)) +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_colour_manual(values = temp_ThreeCols) +
  geom_hline(yintercept = 0) +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))

### Calculate the mean during the intervention period
### 1 - aggregate population density parameter per seed
### 2 - aggregate seed
descMeanReductionVector <- rawdat_combined_cum_long %>%
  dplyr::filter(
    cum == "red",
    PostInterventionDays >= 0 & PostInterventionDays <= 120
  ) %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival, seed) %>%
  summarize(meanRed = mean(value)) %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival) %>%
  summarize(value = mean(meanRed)) %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct) %>%
  f_aggrDat(c("LARVcov", "EIR", "outcome", "outcomeLabel", "Frequency_fct"), "value", WideToLong = FALSE) %>%
  as.data.frame()

plotdatB <- descMeanReductionVector %>% filter(LARVcov %in% c(0, 20, 40, 60, 80, 100) & outcome == "Nv" &
  ((Frequency_fct == "none" & LARVcov != 0) | (Frequency_fct != "none" & LARVcov != 0)))

Figure3 <- ggplot(data = plotdatB) +
  theme_cowplot() +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = LARVcov, linetype = as.factor(LARVcov)), col = "black", size = 1) +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = LARVcov, linetype = as.factor(LARVcov)), col = "white", size = 0, show.legend = FALSE) +
  geom_pointrange(aes(x = Frequency_fct, y = mean.val, ymin = min.val, ymax = max.val, group = as.factor(LARVcov)), col = "cadetblue") +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = LARVcov, linetype = as.factor(LARVcov)), col = "cadetblue", size = 1, show.legend = FALSE) +
  labs(
    title = "B)\n",
    x = "Frequency [every nth day]",
    y = "Mean relative reduction (%)\n[t=0-120]",
    col = "", shape = "Larviciding coverage (%)",
    linetype = "Larviciding coverage (%)"
  ) +
  customTheme_noAngle +
  scale_y_continuous(limits = c(-5, 100)) +
  geom_hline(yintercept = 0) +
  scale_linetype_manual(
    values = c("twodash", "dashed", "dotted", "dotdash", "longdash", "solid"),
    name = "Larviciding\ncoverage (%)",
    labels = c(20, 40, 60, 80, 100),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(legend.position = c(0.75, 0.7)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.key.width = unit(3, "lines"))
print(Figure3)

#### for EIR and prevalence take the reduction at the end of the intervention period.
plotdatC <- subset(Figure1Dat, outcome %in% c("PR", "simulatedEIR") & PostInterventionDays == 120 & (LARVcov == 80 | Frequency == "none"))

Figure2 <- ggplot(data = plotdatC) +
  theme_cowplot() +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = interaction(EIR, outcomeLabel), linetype = as.factor(EIR)), col = "black", size = 1) +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = interaction(EIR, outcomeLabel), linetype = as.factor(EIR)), col = "white", size = 0, show.legend = FALSE) +
  geom_pointrange(aes(
    x = Frequency_fct, y = mean.val, ymin = min.val, ymax = max.val, col = outcomeLabel,
    group = interaction(as.factor(EIR), outcomeLabel)
  ), show.legend = FALSE) +
  geom_line(aes(x = Frequency_fct, y = mean.val, group = interaction(EIR, outcomeLabel), col = outcomeLabel, linetype = as.factor(EIR)), size = 1, show.legend = FALSE) +
  labs(
    title = "C)\n",
    x = "Frequency [every nth day]",
    y = "Relative reduction (%)\n[t=120]",
    col = "", shape = ""
  ) +
  customTheme_noAngle +
  scale_y_continuous(limits = c(-5, 100)) +
  geom_hline(yintercept = 0) +
  theme(legend.position = c(0.75, 0.7)) +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),
    name = "Pre-larviciding\nEIR"
  ) +
  scale_colour_manual(values = temp_ThreeCols) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.key.width = unit(3, "lines"))


# Get legend
fig1legend <- get_legend(ggplot(data = subset(Figure1Dat, EIR == 90 & LARVcov == 80)) +
  geom_line(aes(x = PostInterventionDays, y = mean.val, col = outcomeLabel), size = 1.3) +
  labs(col = "") +
  customTheme_Angle +
  scale_colour_manual(values = temp_ThreeCols) +
  theme_bw() +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ))

### Combine figure and save
Figure32 <- grid.arrange(Figure3, Figure2, nrow = 1, ncol = 2)
Figurecombined <- plot_grid(Figure1, Figure32, nrow = 2, ncol = 1, rel_heights = c(0.45, 0.55))
Figurecombined <- plot_grid(Figurecombined, fig1legend, ncol = 1, rel_heights = c(0.9, 0.1))
print(Figurecombined)

ggsave(paste0("Figure4_", decay, ".png"), plot = Figurecombined, path = file.path("figures"), width = 16, height = 10, device = "png")
# ggsave(paste0("Figure4_", decay, ".pdf"), plot = Figurecombined, path = file.path("figures"), width = 16, height = 10, device = "pdf")


fwrite(plotdatA, file = file.path("figures", "figuredat", paste0("Figure4_", decay, "_A.csv")))
fwrite(plotdatB, file = file.path("figures", "figuredat", paste0("Figure4_", decay, "_B.csv")))
fwrite(plotdatC, file = file.path("figures", "figuredat", paste0("Figure4_", decay, "_C.csv")))
