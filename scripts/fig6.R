## ===================================
## FIGURE 6
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant"
load(file.path("experiments", nameExperiment, "processedExpResults", "DataForAnalysis_Decay5daystep.Rdata"))

## Define grouping variables for aggregation
groupVARSAggr1 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel", "developmentSurvival", "femaleEggsLaidByOviposit", "developmentDuration") ### aggregate density parameter
groupVARSAggr2 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel") ### aggregate seeds

#### Mean across seeds, then aggregate across mosquito density parameters
dat <- rawdat_combined_cum_long %>%
  dplyr::filter(cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)

datx <- dat %>%
  filter(outcome != "Nv0" & PostInterventionDays == 120 &
    Frequency %in% c("1", "2", "3", "4", "6", "12") &
    EIR == 10 & LARVcov != 0) %>%
    dplyr::select(Frequency, Frequency_fct, LARVcov, outcome, PostInterventionDays, mean.val, min.val,max.val)

daty <- dat %>%
  filter(outcome != "Nv0" & PostInterventionDays == 485 &
    Frequency %in% c("1", "2", "3", "4", "6", "12") &
    EIR == 10 & LARVcov != 0)  %>%
  dplyr::select(Frequency, Frequency_fct, LARVcov, outcome, PostInterventionDays, )


datxy <- rbind(datx, daty)
datxy <- datxy %>%
  as.data.frame() %>%
  pivot_wider(
    id_cols = c(outcome, Frequency, Frequency_fct, LARVcov),
    names_from = PostInterventionDays, values_from = c(mean.val, min.val,max.val)
  )

datxy$outcome[datxy$outcome == "PR"] <- "Relative reduction\nin prevalence"
datxy$outcome[datxy$outcome == "simulatedEIR"] <- "Relative reduction\nin EIR"

datxy$outcome2 <- gsub("Relative reduction\nin ", "", datxy$outcome)
datxy$outcome2 <- gsub("prevalence", "Prevalence", datxy$outcome2)


shapeLegend <- get_legend(ggplot(data = subset(datxy, outcome2 == "Prevalence")) +
  geom_point(aes(
    x = mean.val_120, y = mean.val_485, shape = as.factor((Frequency_fct)),
    group = interaction(Frequency_fct, outcome, LARVcov)
  ), size = 5) +
  labs(shape = "Frequency [every nth day]") +
  customTheme_Angle +
  scale_shape_manual(values = c(19, 15, 17, 2, 0, 1)) +
  theme_bw() +
  guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ))


# Get legend
fig1legend <- get_legend(ggplot(data = subset(dat, EIR == 90 & LARVcov == 80 & outcomeLabel != "Mosquito emergence")) +
  geom_line(aes(x = PostInterventionDays, y = mean.val, col = outcomeLabel), size = 1.3) +
  labs(col = "") +
  customTheme_Angle +
  scale_colour_manual(values = temp_ThreeCols) +
  theme_bw() +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ))



pplot_pfpr <- ggplot(data = subset(datxy,  outcome2 == "Prevalence")) +
  theme_cowplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes( x = mean.val_120, ymin = min.val_485, ymax = max.val_485, 
                     group = interaction(Frequency_fct, outcome, LARVcov)),alpha=0.8, col = "grey") +
  geom_errorbarh(aes( y = mean.val_485, xmin = min.val_120, xmax = max.val_120,  
                      group = interaction(Frequency_fct, outcome, LARVcov)),alpha=0.8, col = "grey") +
  geom_point(aes(
    x = mean.val_120, y = mean.val_485, col = as.factor(LARVcov), fill = as.factor(LARVcov), shape = as.factor(Frequency_fct),
    group = interaction(Frequency_fct, outcome, LARVcov)
  ), size = 5) +
  scale_shape_manual(values = c(19, 15, 17, 2, 0, 1)) +
  facet_grid(~outcome2, space = "free") +
  labs(
    shape = "Frequency\n[every nth day]",
    title = "",
    subtitle = "",
    col = "Coverage (%)",
    fill = "Coverage (%)",
    x = "Relative reduction (%)\n[t=120]",
    y = "Relative reduction (%)\n[t=120+365]"
  ) +
  scale_color_manual(values = prev_cols, guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = prev_cols, guide = guide_legend(reverse = TRUE)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  customTheme_noAngle +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain")
  ) +
  scale_y_continuous(breaks=seq(0,35,5), labels=seq(0,35,5), lim=c(-5,35)) +
  guides(shape = FALSE) +
  theme(legend.position = c(0.05, 0.7)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.key.width = unit(3, "lines"))


pplot_eir <- ggplot(data = subset(datxy, outcome2 == "EIR")) +
  theme_cowplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes( x = mean.val_120, ymin = min.val_485, ymax = max.val_485, 
                     group = interaction(Frequency_fct, outcome, LARVcov)),alpha=0.8, col = "grey") +
  geom_errorbarh(aes( y = mean.val_485, xmin = min.val_120, xmax = max.val_120,  
                      group = interaction(Frequency_fct, outcome, LARVcov)),alpha=0.8, col = "grey") +
  geom_point(aes(
    x = mean.val_120, y = mean.val_485, col = as.factor(LARVcov), fill = as.factor(LARVcov), shape = as.factor(Frequency_fct),
    group = interaction(Frequency_fct, outcome, LARVcov)
  ), size = 5) +
  scale_shape_manual(values = c(19, 1, 17, 2, 0, 1)) +
  facet_grid(~outcome2, space = "free") +
  labs(
    shape = "Frequency\n[every nth day]",
    title = "B)",
    subtitle = "",
    col = "Coverage (%)",
    fill = "Coverage (%)",
    x = "Relative reduction (%)\n[t=120]",
    y = "Relative reduction (%)\n[t=120+365]"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain"),
    panel.spacing.x = unit(1, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  customTheme_noAngle +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16),
    strip.text.x = element_text(size = 16, face = "plain"),
    strip.text.y = element_text(size = 16, face = "plain")
  ) +
  scale_y_continuous(breaks=seq(0,35,5), labels=seq(0,35,5), lim=c(-5,35)) +
  scale_color_manual(values = eir_cols, guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = eir_cols, guide = guide_legend(reverse = TRUE)) +
  guides(shape = FALSE) +
  theme(legend.position = c(0.05, 0.7)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  theme(legend.key.width = unit(3, "lines"))


p_top <- ggplot(data = subset(dat, outcome != "Nv0" & PostInterventionDays > 120 &
  Frequency %in% c("1", "2", "3", "4", "6", "12") & EIR == 10 & (LARVcov == 100))) +
  theme_cowplot() +
  geom_ribbon(aes(x = PostInterventionDays, ymin = min.val, ymax = max.val, fill = outcomeLabel), alpha = 0.3) +
  geom_line(aes(x = PostInterventionDays, y = mean.val, col = outcomeLabel), size = 1) +
  labs(
    title = "A)",
    subtitle = "",
    y = "Relative reduction (%)",
    x = "Days since intervention end (t=120+t)",
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
  scale_x_continuous(breaks = seq(120, 510, 90), labels = seq(0, 390, 90)) +
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
  facet_grid(~Frequency_fct, space = "free") +
  customTheme_noAngle +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf))


p_bottom <- plot_grid(pplot_eir, pplot_pfpr + labs(y = ""), nrow = 1)
p_bottom <- plot_grid(p_bottom, shapeLegend, ncol = 1, rel_heights = c(1, 0.1))
p_bottom <- plot_grid(p_bottom, fig1legend, ncol = 1, rel_heights = c(1, 0.1))
(p_all <- plot_grid(p_top, p_bottom, ncol = 1, rel_heights = c(0.6, 1)))


ggsave("Figure6.png", plot = p_all, path = file.path("figures"), width = 13, height = 10, device = "png")
ggsave("Figure6.pdf", plot = p_all, path =  file.path("figures"), width = 13, height = 10, device = "pdf")

fwrite(dat, file = file.path("figures", "figuredat", paste0("Figure6_A.csv")))
fwrite(datxy, file = file.path("figures", "figuredat", paste0("Figure6_B.csv")))


