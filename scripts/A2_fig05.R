## ===================================
## Additional file 2 Fig 05
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant"
load(file.path("experiments", nameExperiment, "processedExpResults", "DataForAnalysis_Decay5daystep.Rdata"))


descMeanReductionVector <- rawdat_combined_cum_long %>%
  dplyr::filter(
    EIR != 10 &
      cum == "red",
    PostInterventionDays >= 0 & PostInterventionDays <= 120
  ) %>%
  dplyr::group_by(
    LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration,
    femaleEggsLaidByOviposit, developmentSurvival, seed
  ) %>%
  dplyr::summarize(meanRed = mean(value)) %>%
  dplyr::group_by(
    LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration,
    femaleEggsLaidByOviposit, developmentSurvival
  ) %>%
  dplyr::summarize(value = mean(meanRed)) %>%
  dplyr::group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct) %>%
  f_aggrDat(c("LARVcov", "EIR", "outcome", "outcomeLabel", "Frequency_fct"), "value", WideToLong = FALSE) %>%
  as.data.frame()

descMeanReductionVector1 <- rawdat_combined_cum_long %>%
  dplyr::filter(
    EIR != 10 &
      cum == "red",
    PostInterventionDays == 0
  ) %>%
  dplyr::mutate(Frequency_fct = "none") %>%
  dplyr::group_by(
    LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration,
    femaleEggsLaidByOviposit, developmentSurvival, seed
  ) %>%
  dplyr::summarize(meanRed = mean(value)) %>%
  dplyr::group_by(
    LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration,
    femaleEggsLaidByOviposit, developmentSurvival
  ) %>%
  dplyr::summarize(value = mean(meanRed)) %>%
  dplyr::group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct) %>%
  f_aggrDat(c("LARVcov", "EIR", "outcome", "outcomeLabel", "Frequency_fct"), "value", WideToLong = FALSE) %>%
  as.data.frame()

descMeanReductionVector2 <- rbind(descMeanReductionVector, descMeanReductionVector1)


dat <- rawdat_combined_cum_long %>%
  dplyr::filter(cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)

### Describe figure
## Mean reductions in Vector density  EIR and prevalence
reductionTable <- descMeanReductionVector2 %>%
  dplyr::group_by(LARVcov, EIR, Frequency_fct, outcomeLabel) %>%
  dplyr::select(outcomeLabel, EIR, LARVcov, Frequency_fct, mean.val) %>%
  spread(Frequency_fct, mean.val) %>%
  arrange(outcomeLabel, EIR, LARVcov)
fwrite(reductionTable, file = file.path("additional_figures", "figuredat", "reductionTable.csv"), sep = ";", row.names = FALSE)



#### Zoomed into figure 1
postLSM1 <- ggplot(data = subset(dat, outcome != "Nv0" & PostInterventionDays >= 120 &
  Frequency %in% c("1", "2", "3", "4", "6", "12") &
  PostInterventionDays >= 0 & EIR == 10 & (LARVcov == 80))) +
  theme_cowplot() +
  geom_ribbon(aes(x = PostInterventionDays, ymin = min.val, ymax = max.val, fill = outcomeLabel), alpha = 0.3) +
  geom_line(aes(x = PostInterventionDays, y = mean.val, col = outcomeLabel), size = 1) +
  labs(
    title = "A) Post-larviciding period",
    subtitle = "Frequency [every nth day]",
    y = "Relative reduction (%)",
    x = "Post-larviciding days",
    col = "", fill = ""
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  scale_x_continuous(breaks = seq(120, 500, 90), labels = seq(0, 380, 90)) +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_colour_manual(values = temp_ThreeCols) +
  geom_hline(yintercept = 0) +
  customTheme_noAngle +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, angle = 90),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain")
  ) +
  facet_wrap(~Frequency_fct, scales = "free") +
  customTheme_noAngle +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))

postLSM2 <- ggplot(data = subset(dat, outcomeLabel %in% c("EIR", "Prevalence") & PostInterventionDays == 500 & Frequency %in% c("1", "2", "3", "4", "6", "12") & PostInterventionDays >= 0 & EIR == 10 & (LARVcov == 80))) +
  theme_cowplot() +
  geom_pointrange(aes(x = Frequency_fct, y = mean.val, ymin = min.val, ymax = max.val, col = outcomeLabel), size = 1) +
  labs(
    title = "B) After 1 year",
    subtitle = "",
    y = "Relative reduction (%)",
    x = "Frequency [every nth day]",
    col = "", fill = ""
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  # scale_x_continuous(breaks=seq(0,130,30)) +
  scale_fill_manual(values = temp_ThreeCols) +
  scale_colour_manual(values = temp_ThreeCols) +
  geom_hline(yintercept = 0) +
  customTheme_noAngle +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, angle = 90),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain")
  ) +
  facet_wrap(~outcomeLabel, nrow = 2) +
  customTheme_noAngle +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))

postLSM <- plot_grid(postLSM1, postLSM2, nrow = 1, rel_widths = c(1.25, 0.5))

postLSM
ggsave(paste0("A2_Fig05", ".png"), plot = postLSM, path = file.path("additional_figures"), width = 17, height = 10, device = "png")
# ggsave(paste0("A2_Fig05", ".pdf"), plot = postLSM, path = file.path("additional_figures"), width = 17, height = 10, device = "pdf")
