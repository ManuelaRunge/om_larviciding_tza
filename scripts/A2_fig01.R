## ===================================
## Additional file 2 Fig 01
## ===================================

source(file.path("scripts/settings.R"))

nameExperiment <- "LSM_SeasonalityTiming_extended"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_cum.Rdata"))
rawdat_combined_cum <- subset(rawdat_combined_cum, SeasonID == "1-1" & Date >= "2001-01-01" & Date <= "2001-12-15")

tempdat <- rawdat_combined_cum %>%
  filter(EIR == 10) %>%
  group_by(Date) %>%
  summarize(
    Nv0 = mean(Nv0),
    Nv = mean(Nv),
    simulated.EIR = mean(simulated.EIR),
    PR = mean(PR)
  )

(tempdat_Nv0 <- tempdat %>% filter(Nv0 == max(Nv0)))
(tempdat_Nv <- tempdat %>% filter(Nv == max(Nv)))
(tempdat_EIR <- tempdat %>% filter(simulated.EIR == max(simulated.EIR)))
(tempdat_PR <- tempdat %>% filter(PR == max(PR)))

tempdat_Nv$Date - tempdat_Nv0$Date
tempdat_EIR$Date - tempdat_Nv$Date
tempdat_PR$Date - tempdat_EIR$Date


as.numeric((tempdat_EIR$Date - tempdat_Nv0$Date)) / 30
as.numeric((tempdat_PR$Date - tempdat_Nv0$Date)) / 30


pplot <- ggplot(data = subset(rawdat_combined_cum, EIR == 10)) +
  theme_bw() +
  geom_smooth(aes(x = Date, y = scale(Nv0), color = "Nv0"), size = 1.3, span = 0.2, se = FALSE) +
  geom_smooth(aes(x = Date, y = scale(Nv), color = "Nv"), size = 1.3, span = 0.2, se = FALSE) +
  geom_smooth(aes(x = Date, y = scale(simulated.EIR), color = "simulated.EIR"), size = 1.3, span = 0.2, se = FALSE) +
  geom_smooth(aes(x = Date, y = scale(PR), color = "PR"), size = 1.3, span = 0.2, se = FALSE) +
  scale_color_manual("",
    breaks = c("Nv0", "Nv", "simulated.EIR", "PR"), values = cols,
    labels = c("Emerging female\nadult mosquitoes", "Host-seeking\nmosquitoes", "EIR", "PfPR")
  ) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  # scale_y_continuous(breaks = c(), labels = c("")) +
  labs(y = "Scaled values", x = "Month", col = "") +
  theme(
    legend.position = c(0.760, 0.875),
    legend.background = element_rect(fill = alpha("white", 0.1))
  ) +
  customTheme_noAngle
print(pplot)


pplotsmall <- ggplot(data = subset(rawdat_combined_cum, EIR == 10)) +
  theme_bw() +
  geom_smooth(aes(x = Date, y = (new.infections / 10), color = "Incidence"), size = 1.3, span = 0.2, se = FALSE) +
  geom_smooth(aes(x = Date, y = (simulated.EIR * 500), color = "simulated.EIR"), size = 1.3, span = 0.2, se = FALSE) +
  labs(y = "Incidence", x = "Month") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 500, name = "daily EIR\n(annual EIR=10)")) +
  scale_color_manual("", values = cols, labels = c("Incidence", "EIR")) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m") +
  theme(legend.position = c(0.75, 0.875), legend.background = element_rect(fill = alpha("white", 0.1))) +
  customTheme_noAngle
print(pplotsmall)

rawdat_combined_cum %>%
  group_by(EIR) %>%
  summarize(PR = mean(PR) * 100)

pplotsmall2 <- ggplot(data = rawdat_combined_cum) +
  theme_bw() +
  geom_smooth(aes(x = Date, y = PR * 100, color = as.factor(EIR), group = EIR), size = 1.3, span = 0.2, se = FALSE) +
  labs(y = "Prevalence (%)", x = "Month", col = "annual EIR") +
  scale_color_manual(values = prev_cols, guide = guide_legend(reverse = TRUE)) +
  scale_x_date(date_breaks = "1 months", date_labels = "%m") +
  scale_y_continuous(lim = c(0, 60)) +
  theme(
    legend.position = c(0.79, 0.725),
    legend.background = element_rect(fill = alpha("white", 0.6))
  ) +
  customTheme_noAngle
print(pplotsmall2)


pp <- plot_grid(pplotsmall, pplotsmall2, ncol = 1, align = "hv", labels = c("B)", "C)"))
p <- plot_grid(pplot, pp, rel_widths = c(1.1, 1), labels = c("A) ", "", ""))

ggsave(paste0("A2_Fig1.png"), plot = p, path = file.path("additional_figures"), width = 12, height = 6, device = "png")
# ggsave(paste0("A2_Fig1.pdf"), plot = p, path = file.path("additional_figures"), width = 12, height = 6, device = "pdf")
