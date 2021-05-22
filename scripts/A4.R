## ===================================
## Additional file 4 Fig 01
## ===================================

source(file.path("scripts/settings.R"))

timelineStart <- "2001-05-01"
timelineEnd <- "2005-08-01"
interventionStart <- "2002-06-01"
interventionEnd <- "2004-09-01"

groupVars <- c("months_nr", "year", "months", "BsBtiCov", "Date", "rainfall")
outcomeVars <- c("Nv", "Nv0", "Ov", "Sv", "simulated.EIR", "prev")
keepVars <- c(groupVars, outcomeVars)

nameExperiment <- "LSM_Fillinger2006_A"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_mth_edited.Rdata"))

rawdat_combined_mthAggrA <- rawdat_combined_mth %>%
  dplyr::select_(.dots = keepVars) %>%
  dplyr::group_by(.dots = groupVars) %>%
  summarize_all(.funs = mean) %>%
  as.data.frame()
rm(rawdat_combined_mth)


nameExperiment <- "LSM_Fillinger2006_B"
load(file.path("experiments", nameExperiment, "processedExpResults", "rawdat_combined_mth_edited.Rdata"))
rawdat_combined_mth$months_nr <- as.numeric(rawdat_combined_mth$months_nr)

rawdat_combined_mthAggr <- rawdat_combined_mth %>%
  dplyr::select_(.dots = keepVars) %>%
  dplyr::group_by(.dots = groupVars) %>%
  summarize_all(.funs = mean) %>%
  as.data.frame()


#### Seasonality
rainfall <- c(238, 100, 283, 500, 471, 240, 131, 126, 164, 145, 316, 319)
months_nr <- c(1:12)
seasonality <- as.data.frame(cbind(months_nr, rainfall))


seasonalityDat <- seasonality %>%
  left_join(unique(rawdat_combined_mth[, c("Date", "months_nr", "months", "year")])) %>%
  arrange(Date)

dummy <- mutate(seasonalityDat[which(seasonalityDat$Date %in% as.Date(c("2001-01-01", "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01"))), ], Date = Date - 1L, year = year(Date))
seasonalityDat <- as.data.frame(rbind(seasonalityDat, dummy))

#### with seasonality   --- emerging larvae (Figure 5)
# https://stackoverflow.com/questions/42150690/ggplot-clean-way-to-show-year-once-and-group-months-within
pplotA <- ggplot(data = subset(rawdat_combined_mthAggrA, Date >= timelineStart & Date <= timelineEnd & BsBtiCov %in% c("90-90"))) +
  theme_cowplot() +
  geom_area(
    data = subset(seasonalityDat, Date >= timelineStart & Date <= timelineEnd),
    aes(x = Date, y = rainfall * 0.01), fill = "black", group = 1, size = 1
  ) +
  geom_bar(aes(x = Date, y = Nv0 / nHost), stat = "identity", fill = "gray", col = "black") +
  labs(
    title = "B) With retreatment intervals (simulated)",
    subtitle = "",
    x = "",
    y = "Emerging larvae per 10'000"
  ) +
  geom_segment(
    x = as.Date(interventionStart), xend = as.Date(interventionStart),
    y = 50000 / nHost, yend = 30000 / nHost, size = 2, arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    x = as.Date(interventionEnd), xend = as.Date(interventionEnd),
    y = 50000 / nHost, yend = 30000 / nHost, size = 2, arrow = arrow(length = unit(0.5, "cm"))
  ) +
  theme(
    panel.spacing.x = unit(0, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white"),
    axis.text.x = element_text(size = 16, angle = 90, hjust = 0, vjust = 0),
    plot.title = element_text(size = 18)
  ) +
  scale_x_date(labels = date_format("%b"), date_breaks = "2 month", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~ . / 0.01, name = "Rainfall in mm"))


#### with seasonality   --- emerging larvae (Figure 5)
pplotB <- ggplot(data = subset(rawdat_combined_mth, seed == 1 & Date >= timelineStart & Date <= timelineEnd & LARVcov == 95 & LARVcov_dry == 0 & LARVcov_rain == 0)) +
  theme_cowplot() +
  geom_area(
    data = subset(seasonalityDat, Date >= timelineStart & Date <= timelineEnd),
    aes(x = Date, y = rainfall * 0.01), fill = "black", group = 1, size = 1
  ) +
  geom_bar(aes(x = Date, y = Nv0 / nHost), stat = "identity", fill = "gray", col = "black") +
  labs(
    title = "C) Constant reduction (simulated)",
    subtitle = "",
    x = "",
    y = "Emerging larvae per 10'000"
  ) +
  geom_segment(
    x = as.Date(interventionStart), xend = as.Date(interventionStart),
    y = 50000 / nHost, yend = 30000 / nHost, size = 2, arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    x = as.Date(interventionEnd), xend = as.Date(interventionEnd),
    y = 50000 / nHost, yend = 30000 / nHost, size = 2, arrow = arrow(length = unit(0.5, "cm"))
  ) +
  theme(
    panel.spacing.x = unit(0, "line"),
    strip.placement = "outside",
    strip.background = element_rect(colour = "black", fill = "white"),
    axis.text.x = element_text(size = 16, angle = 90, hjust = 0, vjust = 0),
    plot.title = element_text(size = 18)
  ) +
  scale_x_date(labels = date_format("%b"), date_breaks = "2 month", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~ . / 0.01, name = "Rainfall in mm"))

pplotAB <- plot_grid(pplotA, pplotB, ncol = 2)

library(png)
img <- readPNG(file.path("experiments", nameExperiment, "Fillinger_2006_adultDensity_cut.PNG"))
g <- rasterGrob(img, interpolate = TRUE)
g <- arrangeGrob(g, top = textGrob("A) With re-treatment intervals (observed)",
  gp = gpar(fontsize = 18, fontface = "bold")
))

pplotABC <- plot_grid(g, pplotAB, ncol = 1, rel_heights = c(1, 0.8), rel_widths = c(1, 1))

ggsave(paste0("A4_Fig01.png"), plot = pplotABC, path = file.path("additional_figures"), width = 16, height = 13, device = "png")
# ggsave(paste0("A4_Fig01.pdf"), plot = pplotABC, path = file.path("additional_figures"),  width = 16, height = 13, device = "pdf")
