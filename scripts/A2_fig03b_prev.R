## ===================================
## Additional file 2 Fig 03 - outcome Prevalence
## ===================================
source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_constant_duration"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))
load(file.path("experiments", nameExperiment, "processedExpResults", "DataForAnalysis.Rdata"))
colnames(rawdat_combined_red) <- gsub("[.]", "", colnames(rawdat_combined_red))

rawdat_combined_redAggrSeeds <- rawdat_combined_red %>%
  dplyr::group_by(PostInterventionDays, LARVcov, EIR, Duration, developmentSurvival, developmentDuration, femaleEggsLaidByOviposit) %>%
  dplyr::summarize(simulatedEIR = mean(simulatedEIR), Nv = mean(Nv), Nv0 = mean(Nv0), PR = mean(PR)) %>%
  mutate(LARVcovLabel = paste0("Coverage ", LARVcov, "%"))


rawdat_combined_redAggrSeeds$LARVcovLabel <- factor(rawdat_combined_redAggrSeeds$LARVcovLabel,
  levels = unique(paste0("Coverage ", rawdat_combined_redAggrSeeds$LARVcov, "%")),
  labels = unique(paste0("Coverage ", rawdat_combined_redAggrSeeds$LARVcov, "%"))
)



f_customplot <- function(dat, selectedParam, outcomeVar) {
  tempdat_prep <- as.data.frame(dat)

  colnames(tempdat_prep)[colnames(tempdat_prep) == outcomeVar] <- "outcomeVar"
  if (selectedParam == "developmentSurvival") tempdat_prep$developmentSurvival <- tempdat_prep$developmentSurvival / 100
  tempdat <- tempdat_prep %>%
    filter(PostInterventionDays <= 150 & LARVcov == 100 & Duration == 2) %>%
    f_aggrDat(groupVars = c("PostInterventionDays", "EIR", "LARVcov", "Duration", selectedParam), valueVar = "outcomeVar", WideToLong = FALSE)

  if (selectedParam == "developmentSurvival") selectedLabel <- "development\nsurvival"
  if (selectedParam == "femaleEggsLaidByOviposit") selectedLabel <- "female eggs\nlaid by oviposit"
  if (selectedParam == "developmentDuration") selectedLabel <- "development\nduration"

  if (outcomeVar == "Nv0") outcomeVarLabel <- "Emerging adult mosquitoes\n(Nv0/nHost)"
  if (outcomeVar == "Nv") outcomeVarLabel <- "Host-seeking mosquitoes\n(Nv/nHost)"
  if (outcomeVar == "simulatedEIR") outcomeVarLabel <- "EIR"
  if (outcomeVar == "PR") outcomeVarLabel <- "PR"

  tempdat2 <- tempdat_prep %>%
    filter(PostInterventionDays <= 150 & EIR == 10 & LARVcov == 100 & Duration == 2) %>%
    f_aggrDat(groupVars = c("PostInterventionDays", "EIR", "LARVcov", "Duration"), valueVar = "outcomeVar", WideToLong = FALSE)

  colnames(tempdat)[colnames(tempdat) == selectedParam] <- "MainParam"

  pplot <- ggplot(data = tempdat) +
    geom_ribbon(aes(
      x = PostInterventionDays, ymin = lower.ci.val, ymax = upper.ci.val, fill = as.factor(MainParam)
    ), alpha = 0.5) +
    geom_line(aes(x = PostInterventionDays, y = mean.val, col = as.factor(MainParam)), size = 1) +
    labs(y = outcomeVarLabel, x = "Post-intervention days") +
    theme_cowplot() +
    background_grid() +
    scale_x_continuous(breaks = seq(0, 150, 30), labels = seq(0, 150, 30)) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    labs(fill = selectedLabel, color = selectedLabel) +
    theme(
      legend.position = c(0.725, 0.2),
      plot.title = element_text(size = 12, hjust = 0)
    ) +
    customTheme_noAngle +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  return(pplot)
}


for (selected_EIR in c(3, 10, 90)) {
  plotdat <- subset(rawdat_combined_redAggrSeeds, EIR == selected_EIR)

  p1 <- f_customplot(plotdat, "developmentSurvival", "PR") +
    labs(title = "A) Development survival")
  p2 <- f_customplot(plotdat, "femaleEggsLaidByOviposit", "PR") +
    labs(y = "", title = "B) Eggs laid per feeding-ovipositing-cycle")
  p3 <- f_customplot(plotdat, "developmentDuration", "PR") +
    labs(y = "", title = "C) Development duration")


  p123_Nv0 <- plot_grid(p1, p2, p3, nrow = 1)


  p1 <- f_customplot(plotdat, "developmentSurvival", "PR") +
    labs(title = "D) Development survival")
  p2 <- f_customplot(plotdat, "femaleEggsLaidByOviposit", "PR") +
    labs(y = "", title = "E) Eggs laid per feeding-ovipositing-cycle")
  p3 <- f_customplot(plotdat, "developmentDuration", "PR") +
    labs(y = "", title = "F) Development duration")



  p123_Nv <- plot_grid(p1, p2, p3, nrow = 1)
  p123 <- plot_grid(p123_Nv0, p123_Nv, ncol = 1)

  ggsave(paste0("A2_Fig3b_PR_EIR-", selected_EIR, ".png"), plot = p123, path = file.path("additional_figures"), width = 18, height = 12, device = "png")
  # ggsave(paste0("A2_Fig3b_PR_EIR-",selected_EIR, ".pdf"), plot = p123, path = file.path("additional_figures"),width = 18, height = 12, device = "pdf")
}
