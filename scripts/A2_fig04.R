## ===================================
## Additional file 2 Fig 04
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant"
decay <- "Decay5daystep"

load(file.path("experiments", nameExperiment, "processedExpResults", paste0("DataForAnalysis_", decay, ".Rdata")))

## Define grouping variables for aggregation
groupVARSAggr1 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel", "developmentSurvival", "femaleEggsLaidByOviposit", "developmentDuration") ### aggregate density parameter
groupVARSAggr2 <- c("PostInterventionDays", "Decay2", "EIR", "Frequency", "Frequency_fct", "LARVcov", "outcome", "outcomeLabel") ### aggregate seeds

## exclude every 25 days (n initial applications =5) to simplify plot
rawdat_combined_cum_long <- subset(rawdat_combined_cum_long, Frequency != "5")

#### take mean of seeds, then aggregate across density dependency parameters
Figure1Dat <- rawdat_combined_cum_long %>%
  dplyr::filter(cum == "red") %>%
  dplyr::group_by_(.dots = groupVARSAggr1) %>%
  dplyr::summarize(value = mean(value)) %>%
  f_aggrDat(groupVARSAggr2, "value", WideToLong = FALSE)


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

descMeanReductionVector1 <- rawdat_combined_cum_long %>%
  dplyr::filter(
    cum == "red",
    PostInterventionDays == 0
  ) %>%
  dplyr::mutate(Frequency_fct = "none") %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival, seed) %>%
  summarize(meanRed = mean(value)) %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival) %>%
  summarize(value = mean(meanRed)) %>%
  group_by(LARVcov, EIR, outcome, outcomeLabel, Frequency_fct) %>%
  f_aggrDat(c("LARVcov", "EIR", "outcome", "outcomeLabel", "Frequency_fct"), "value", WideToLong = FALSE) %>%
  as.data.frame()


descMeanReductionVector2 <- rbind(descMeanReductionVector, descMeanReductionVector1)
plotdatB <- descMeanReductionVector2 %>% filter(LARVcov %in% c(0, 20, 40, 60, 80, 100) & outcome == "Nv" & ((Frequency_fct == "none" & LARVcov != 0) | (Frequency_fct != "none" & LARVcov != 0)))
plotdatC <- subset(Figure1Dat, outcome %in% c("PR", "simulatedEIR") & PostInterventionDays == 120 & (LARVcov == 80 | Frequency == "none"))


freqcovdat <- Figure1Dat %>%
  filter(outcome == "PR", PostInterventionDays == 120) %>%
  mutate(mean.val = round(mean.val, 0))
matdat <- freqcovdat %>%
  dplyr::select(Frequency_fct, LARVcov, Decay2, mean.val, EIR) %>%
  arrange(mean.val)

matdat$decayLabel <- gsub("_", "\n", matdat$Decay2)

# https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
matdatp <- ggplot(matdat, aes(LARVcov, Frequency_fct)) +
  theme_cowplot() +
  geom_tile(aes(fill = mean.val)) +
  geom_text(aes(label = mean.val), size = 5) +
  scale_fill_gradient(low = "white", high = "red") +
  facet_wrap(~EIR, labeller = labeller(EIR = label_both), scales = "free") +
  labs(
    title = "",
    x = "Coverage (%)",
    y = "Frequency [every nth day]",
    fill = "Relative\nreduction (%)",
    col = "", shape = "",
    linetype = "Pre-larviciding\nEIR"
  ) +
  customTheme_noAngle +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = seq(0, 100, 20))

ggsave(paste0("A2_Fig04.png"), plot = matdatp, path = file.path("additional_figures"), width = 14, height = 5, device = "png")
# ggsave(paste0("A2_Fig4.pdf"), plot = matdatp, path = file.path("additional_figures"), width = 14, height = 5, device = "pdf")


fwrite(matdat, file = file.path("additional_figures", "figuredat", "A2_Fig4.csv"))

##### For text
#### reductions per outcome per frequency

plotdatA %>%
  filter(PostInterventionDays == 120) %>%
  dplyr::mutate(mean.val = mean.val) %>%
  dplyr::select(outcome, Frequency_fct, mean.val) %>%
  spread(Frequency_fct, mean.val)

plotdatB %>%
  filter(LARVcov == 80) %>%
  dplyr::mutate(mean.val = mean.val) %>%
  dplyr::select(EIR, outcome, Frequency_fct, mean.val) %>%
  spread(Frequency_fct, mean.val)

plotdatC %>%
  filter(LARVcov == 80) %>%
  dplyr::mutate(mean.val = mean.val) %>%
  dplyr::select(EIR, outcome, Frequency_fct, mean.val) %>%
  spread(Frequency_fct, mean.val)
