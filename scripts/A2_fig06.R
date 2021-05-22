## ===================================
## Additional file 2 Fig 06
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant_duration"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_red.Rdata"))
rawdat_combined_red <- rawdat_combined_red %>% filter(Duration == 4 & LARVcov %in% c(0, 20, 40, 60, 80, 100))
rawdat_combined_red <- rawdat_combined_red %>% filter(PostInterventionDays <= 365)

## aggregate seeds
groupVARS <- c(
  "Duration", "PostInterventionDays", "EIR", "LARVcov"
)


### Take mean across seeds, then summarize density dependency parameters
rawdat_combined_redAggr <- rawdat_combined_red %>%
  group_by(
    Duration, PostInterventionDays, EIR, LARVcov,
    developmentDuration, developmentSurvival, femaleEggsLaidByOviposit
  ) %>%
  summarize(red_PR = mean(red_PR)) %>%
  dplyr::group_by_(.dots = groupVARS) %>%
  f_aggrDat(groupVARS, "red_PR")

outcomeLabel <- "Relative reduction in prevalence (%)\n[t=0 - 365]"


pplotB <- ggplot(data = rawdat_combined_redAggr) +
  theme_cowplot() +
  background_grid() +
  annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_ribbon(
    aes(
      x = PostInterventionDays,
      ymin = min.val, ymax = max.val,
      fill = as.factor(LARVcov),
      group = interaction(LARVcov)
    ),
    size = 1, alpha = 0.6
  ) +
  geom_line(aes(
    x = PostInterventionDays,
    y = mean.val,
    col = as.factor(LARVcov),
    group = interaction(LARVcov)
  ), size = 1) +
  labs(
    y = outcomeLabel,
    x = "Days since intervention start (t)",
    fill = "Coverage (%)",
    col = "Coverage (%)"
  ) +
  facet_wrap(~EIR, labeller = labeller(EIR = label_both)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold"),
    strip.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold")
  ) +
  scale_color_viridis_d(option = "A", end = 0.9) +
  scale_fill_viridis_d(option = "A", end = 0.9) +
  scale_x_continuous(
    breaks = c(0, 60, 120, 180, 240, 300, 365),
    labels = c(0, 60, 120, 180, 240, 300, 365)
  ) +
  geom_hline(yintercept = c(-Inf, 0, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "right")


### Take mean across seeds, then summarize density dependency parameters
rawdat_combined_redAggr <- rawdat_combined_red %>%
  group_by(
    Duration, PostInterventionDays, EIR, LARVcov,
    developmentDuration, developmentSurvival, femaleEggsLaidByOviposit
  ) %>%
  summarize(PR = mean(PR) * 100) %>%
  dplyr::group_by_(.dots = groupVARS) %>%
  f_aggrDat(groupVARS, "PR")

outcomeLabel <- "Predicted  prevalence (%)\n[t=0 - 365]"


pplotA <- ggplot(data = rawdat_combined_redAggr) +
  theme_cowplot() +
  background_grid() +
  annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  geom_ribbon(
    aes(
      x = PostInterventionDays,
      ymin = min.val, ymax = max.val,
      fill = as.factor(LARVcov),
      group = interaction(LARVcov)
    ),
    size = 1, alpha = 0.6
  ) +
  geom_line(aes(
    x = PostInterventionDays,
    y = mean.val,
    col = as.factor(LARVcov),
    group = interaction(LARVcov)
  ), size = 1) +
  labs(
    y = outcomeLabel,
    x = "Days since intervention start (t)",
    fill = "Coverage (%)",
    col = "Coverage (%)"
  ) +
  facet_wrap(~EIR, labeller = labeller(EIR = label_both)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold"),
    strip.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5, face = "bold")
  ) +
  scale_color_viridis_d(option = "A", end = 0.9) +
  scale_fill_viridis_d(option = "A", end = 0.9) +
  scale_x_continuous(
    breaks = c(0, 60, 120, 180, 240, 300, 365),
    labels = c(0, 60, 120, 180, 240, 300, 365)
  ) +
  geom_hline(yintercept = c(-Inf, 0, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.position = "right")


pplot <- plot_grid(pplotA, pplotB, nrow = 2, labels = c("A)", "B)"))

ggsave(paste0("A2_Fig06.png"), plot = pplot, path = file.path("additional_figures"), width = 12, height = 8, device = "png")
# ggsave(paste0("A2_Fig06.pdf"), plot = pplot, path = file.path("additional_figures"), width = 12, height = 8, device = "pdf")
