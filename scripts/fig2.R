## ===================================
## FIGURE 2
## ===================================

source(file.path("scripts/settings.R"))

load(file.path("dat", "TimestepDat.RData"))

InterventionStart <- "2002-01-01"
InterventionEnd <- "2005-01-01"


timelineAll <- seq(as.Date(InterventionStart), as.Date(InterventionEnd), "days")
timeDatAll <- as.data.frame(cbind(as.character(timelineAll), "0"))
colnames(timeDatAll) <- c("date", "zero")

dataList_v2_dat <- do.call("rbind", dataList_v2) # combine all vectors into a matrix
dataList_v1_dat <- do.call("rbind", dataList_v1) # combine all vectors into a matrix
dataList_dat <- do.call("rbind", dataList) # combine all vectors into a matrix

dataList_v2_dat$date <- as.character(dataList_v2_dat$Dates)
dataList_v2_dat$name <- rownames(dataList_v2_dat)
rownames(dataList_v2_dat) <- c(1:dim(dataList_v2_dat)[1])

suppressWarnings(dataList_v2_dat <- separate(dataList_v2_dat, "name", into = c("name", "num"), sep = "[.]"))
dataList_v2_dat$name <- gsub("timedat12_", "every_", dataList_v2_dat$name)

Labels <- c("every_firsttimesteps", "every_12timesteps", "every_6timesteps", "every_5timesteps", "every_4timesteps", "every_3timesteps", "every_2timesteps", "every_1timesteps")
Labels2 <- rev(c("every 5 days", "every 10 days", "every 15 days", "every 20 days", "every 25 days", "every 30 days", "every 60 days", "once"))

dataList_v2_dat <- dataList_v2_dat %>% mutate(num = ifelse(!is.na(num), num, 1))
dataList_v2_dat$name_fct <- factor(dataList_v2_dat$name,
  levels = Labels,
  labels = Labels
)


timeDatAll <- merge(timeDatAll, dataList_v2_dat, by = "date", all.x = TRUE)
timeDatAll$date <- as.Date(timeDatAll$date)

counter <- icount()
timeDatAll <- timeDatAll %>%
  group_by(date) %>%
  dplyr::mutate(days = as.numeric(nextElem(counter))) %>%
  as.data.frame()
timeDatAll$effectDuration <- timeDatAll$days + 5
timeDatAll$effectDuration[is.na(timeDatAll$name)] <- NA

## remove every 25 days
if (removeEvery5Days) {
  timeDatAll <- subset(timeDatAll, name != "every_5timesteps")
  Labels <- c("every_firsttimesteps", "every_12timesteps", "every_6timesteps", "every_4timesteps", "every_3timesteps", "every_2timesteps", "every_1timesteps")
  Labels2 <- rev(c("every 5 days", "every 10 days", "every 15 days", "every 20 days", "every 30 days", "every 60 days", "once"))

  timeDatAll$name_fct <- factor(timeDatAll$name,
    levels = Labels,
    labels = Labels
  )
}

labelDat <- timeDatAll %>%
  dplyr::select(name, name_fct, num) %>%
  unique() %>%
  group_by(name, name_fct) %>%
  mutate(nrounds = max(as.numeric(num)), effectDuration = NA, days = as.numeric(140)) %>%
  select(-num) %>%
  unique()

class(timeDatAll$days)
class(labelDat$days)
nlevel <- length(unique(timeDatAll$name_fct))

deployDat <- subset(timeDatAll, !is.na(name), date <= max(dataList_v2_dat$Dates))

pdeployDat <- ggplot(
  data = deployDat,
  aes(ymin = days - 1, ymax = effectDuration - 1, y = days - 1, x = as.numeric(name_fct))
) +
  theme_cowplot() +
  geom_vline(xintercept = c(1:nlevel), size = 0.5, col = "grey") +
  geom_pointrange(col = "deepskyblue2", size = 1) +
  geom_point(shape = 23, size = 3, fill = "black", col = "white") +
  scale_y_continuous(breaks = seq(5, 130, 5), labels = seq(0, 125, 5)) +
  scale_x_continuous(
    name = "Frequency\n", breaks = c(1:nlevel), labels = Labels2,
    sec.axis = dup_axis(
      breaks = c(1:nlevel),
      labels = rev(c("25", "13", "9", "7", "5", "3", "1")),
      name = "n deployments"
    ),
  ) +
  labs(title = "Application rate", subtitle = "", y = "Time (days)") +
  coord_flip() +
  customTheme_Angle
print(pdeployDat)


### ------------------------------------
### create Season Dat
### ------------------------------------
## as estimated by Stuckey
SeasonalID <- matrix(NA, 4, 6)
colnames(SeasonalID) <- c("0", "1-1", "2-1", "1-2", "0.5-2", "2-2")
rownames(SeasonalID) <- c("a1", "a2", "b1", "b2")

Label <- c(
  "No seasonality",
  "Medium, one season",
  "High, one season",
  "Medium, two seasons",
  "Low, two seasons",
  "High, two seasons"
)

SeasonalID_description <- as.data.frame(cbind(SeasonID = paste0("SeasonID_", colnames(SeasonalID)), Label))
SeasonalID_description$SeasonID <- as.character(SeasonalID_description$SeasonID)

## from additional file Stuckey et al 2012
SeasonalID[, 1] <- c(0, 0, 0, 0)
SeasonalID[, 2] <- c(1.76256, 0, 0, 0)
SeasonalID[, 3] <- c(4.10688, 0, 0, 0)
SeasonalID[, 4] <- c(0.836862, 0.836862, 0, 0)
SeasonalID[, 5] <- c(0.437636, 0.437636, 0, 0)
SeasonalID[, 6] <- c(2.05344, 2.05344, 0, 0)


t2 <- c(1:365)
f_getDailyEIR(SeasonalID[, 2], t2, selected_aEIR = 3)

EIRs <- c(3, 10, 90)
SeasonalDat <- matrix(NA, (length(EIRs) * length(t2)), ncol(SeasonalID) + 2)
SeasonalDat[, 1] <- rep(t2, length(EIRs))
SeasonalDat[, 2] <- rep(EIRs, each = length(t2))
colnames(SeasonalDat) <- c("Timestep", "EIR", paste0("SeasonID_", colnames(SeasonalID)))

for (j in 1:length(EIRs)) {
  for (i in 1:ncol(SeasonalID)) {
    SeasonalDat[SeasonalDat[, 2] == EIRs[j], i + 2] <- f_getDailyEIR(SeasonalID[, i], t2, EIRs[j])
  }
}


SeasonalDat <- as.data.frame(SeasonalDat) %>%
  melt(id.vars = c("Timestep", "EIR")) %>%
  mutate(SeasonID = as.character(variable)) %>%
  select(-variable) %>%
  left_join(SeasonalID_description, by = "SeasonID")

numVars <- c("EIR", "Timestep", "value")
SeasonalDat[, numVars] <- lapply(SeasonalDat[, numVars], as.numeric)

### MONTHLY SHIFTS !!! - TO LABEL PEAK IN EIR , TRUE MONTHS AND RAINFALL MONTHS
SeasonalDat$month <- as.numeric(with(SeasonalDat, format(strptime(paste(Timestep), format = "%j"), "%m")))

SeasonalDat$rainfall_mth <- SeasonalDat$month - 1
SeasonalDat$rainfall_mth[SeasonalDat$rainfall_mth == 0] <- 12

SeasonalDat$rainfall_mth_adj <- factor(SeasonalDat$rainfall_mth,
  levels = c(10:12, 1:9),
  labels = c(-6:5)
)

SeasonalDat$month_adj <- factor(SeasonalDat$month,
  levels = c(10:12, 1:9),
  labels = c(-6:5)
)

SeasonalDat_mth <- SeasonalDat %>%
  dplyr::group_by(EIR, month, rainfall_mth, month_adj, rainfall_mth_adj, SeasonID, Label) %>%
  dplyr::summarize(seasonality = mean(value))

### Add seasonal timing to plot , for 1 selected seasonal pattern

SeasonalDat_mth$WetSeason <- NA
SeasonalDat_mth$DrySeason <- NA
SeasonalDat_mth$EndOfDrySeason <- NA
SeasonalDat_mth$EndOfRainSeason <- NA

#### True starting points - relative to rainfall!
SeasonalDat_mth$WetSeason[SeasonalDat_mth$rainfall_mth_adj %in% c(-4:0)] <- 1 # c(-3:0)
SeasonalDat_mth$DrySeason[SeasonalDat_mth$rainfall_mth_adj %in% c(2:5, -6)] <- 1 # c(3:6)
SeasonalDat_mth$EndOfDrySeason[SeasonalDat_mth$rainfall_mth_adj %in% c(-6:-2)] <- 1 ## Begin of rain    #c(-5:-2)
SeasonalDat_mth$EndOfRainSeason[SeasonalDat_mth$rainfall_mth_adj %in% c(-1:3)] <- 1 # c(-1:2)

SeasonalDat_mth$year <- 2002
SeasonalDat_mth2 <- SeasonalDat_mth
SeasonalDat_mth2$year <- 2003
SeasonalDat_mth <- rbind(SeasonalDat_mth, SeasonalDat_mth2)

SeasonalDat_long <- melt(as.data.frame(SeasonalDat_mth), id.vars = c("EIR", "year", "month", "month_adj", "rainfall_mth", "rainfall_mth_adj", "SeasonID", "Label", "seasonality"))
SeasonalDat_long$value[SeasonalDat_long$variable == "WetSeason"] <- SeasonalDat_long$value[SeasonalDat_long$variable == "WetSeason"] / 13.75
SeasonalDat_long$value[SeasonalDat_long$variable == "DrySeason"] <- SeasonalDat_long$value[SeasonalDat_long$variable == "DrySeason"] / 13.75
SeasonalDat_long$value[SeasonalDat_long$variable == "EndOfDrySeason"] <- SeasonalDat_long$value[SeasonalDat_long$variable == "EndOfDrySeason"] / 11.75
SeasonalDat_long$value[SeasonalDat_long$variable == "EndOfRainSeason"] <- SeasonalDat_long$value[SeasonalDat_long$variable == "EndOfRainSeason"] / 11.5

SeasonalDat_long$season_corrected <- as.character(SeasonalDat_long$variable)
SeasonalDat_long$season_corrected[SeasonalDat_long$variable == "WetSeason"] <- "Mid of rain"
SeasonalDat_long$season_corrected[SeasonalDat_long$variable == "DrySeason"] <- "Dry season"
SeasonalDat_long$season_corrected[SeasonalDat_long$variable == "EndOfDrySeason"] <- "Begin of rain"
SeasonalDat_long$season_corrected[SeasonalDat_long$variable == "EndOfRainSeason"] <- "End of rain"
SeasonalDat_long$season_corrected[SeasonalDat_long$variable == "Throughout the year"] <- "Throughout the year"

SeasonalDat_longDUMMY <- subset(SeasonalDat_long, variable == "WetSeason")
SeasonalDat_longDUMMY$variable <- "Throughout the year"
SeasonalDat_longDUMMY$value <- 0.1
SeasonalDat_longDUMMY$season_corrected <- "Throughout the year"
SeasonalDat_long <- as.data.frame(rbind(SeasonalDat_long, SeasonalDat_longDUMMY))


### just for labels, which are shifted in plot due to the length of the text, these values to now show when the season starts!
LabelDat <- SeasonalDat_long %>%
  filter(SeasonID == "SeasonID_1-1" & year == 2002 &
    (month_adj %in% c(-3) & season_corrected == "Throughout the year") | # c(-3)
    (month_adj %in% c(-4) & season_corrected == "Begin of rain") | # c(-4)
    (month_adj %in% c(-2) & season_corrected == "Mid of rain") | # c(-2)
    (month_adj %in% c(1) & season_corrected == "End of rain") | # c(0)
    (month_adj %in% c(4) & season_corrected == "Dry season")) %>% # c(4)
  select(variable, season_corrected, month_adj, rainfall_mth) %>%
  unique()
LabelDatII <- SeasonalDat_long %>%
  filter(!is.na(value)) %>%
  select(season_corrected, value) %>%
  unique()
LabelDat <- LabelDat %>% left_join(LabelDatII, by = "season_corrected")

SeasonalDat_long <- subset(SeasonalDat_long, EIR == 10 & year == 2002)
arrowdat <- as.data.frame(cbind("x" = c(1:12), "xend" = c(1:12), "y" = rep(0.12, 12), "yend" = rep(0.11, 12)))

raindat <- SeasonalDat_long %>%
  filter(SeasonID == "SeasonID_1-1") %>%
  mutate(month_adj = rainfall_mth_adj, rain = seasonality) %>%
  select(month_adj, rain)

SeasonalDat_long <- left_join(SeasonalDat_long, raindat, by = "month_adj")


pseasonDeploy <- ggplot(data = subset(SeasonalDat_long, SeasonID == "SeasonID_1-1")) +
  theme_cowplot() +
  geom_area(
    data = subset(SeasonalDat_long, variable == "WetSeason" & SeasonID == "SeasonID_1-1"),
    aes(x = month_adj, y = seasonality, group = 1), stat = "identity", position = "dodge", alpha = 0.3
  ) +
  # geom_line(aes(x = month_adj, y = rain, group = 1), size = 1.7) +
  geom_line(aes(x = month_adj, y = value, col = season_corrected, group = season_corrected), size = 1.7) +
  geom_label(
    data = LabelDat,
    aes(x = month_adj, y = value, col = season_corrected, label = season_corrected),
    size = 5, nudge_x = 0, nudge_y = 0.005, show.legend = NA, label.size = NA, stat = "identity"
  ) +
  labs(title = "Seasonal deployment", subtitle = "", x = "Months relative to highest peak", y = "", col = "Deployment") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) +
  scale_alpha_discrete(range = c(0.15, 0.25)) +
  geom_text(x = 6, y = 0.01, label = "TRANSMISSION (EIR)", col = "grey39") +
  scale_y_continuous(limits = c(0, 0.12), expand = c(0, 0)) +
  scale_color_manual(values = FiveCols) +
  geom_segment(
    data = arrowdat, aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.2, "cm")), size = 0.5
  ) +
  geom_hline(yintercept = c(-Inf, Inf))
print(pseasonDeploy)

#### Repduce EIR seasonality plot as in
# 1. Stuckey EM, Smith T, Chitnis N. Seasonally Dependent Relationships between Indicators of Malaria Transmission
# and Disease Provided by Mathematical Model Simulations. PLOS Computational Biology. 2014 Sep 4;10(9):e1003812.

pseason <- ggplot(
  data = subset(SeasonalDat, SeasonID != "SeasonID_0.5-2"),
  aes(x = Timestep, y = value, col = Label)
) +
  theme_cowplot() +
  geom_line(size = 1.5) +
  facet_grid(~EIR, labeller = "label_both") +
  customTheme_noAngle +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_vline(xintercept = c(-Inf, Inf)) +
  scale_x_continuous(labels = c()) +
  scale_y_log10(labels = c(), breaks = c()) +
  labs(title = "Seasonality", subtitle = "", linetype = "EIR", x = "Seasonality", y = "Daily EIR (log10)", color = "") +
  scale_color_manual(values = SeasonCols) +
  theme(legend.position = "bottom", axis.ticks.x = element_blank()) +
  guides(color = guide_legend(ncol = 3)) +
  customTheme_noAngle


monthAdjustDat <- as.data.frame(unique(SeasonalDat_long[, c("month", "month_adj", "rainfall_mth", "rainfall_mth_adj")]))
save(LabelDat, monthAdjustDat, SeasonalDat, SeasonalDat_long, arrowdat, file = file.path("dat", "SeasonalDat.Rdata"))



### Add coverage
covd <- seq(0, 100, 10)
boxd <- rep(c(0, 100), each = length(covd))
dummydat <- as.data.frame(cbind(xval = "1", covd, boxd))
dummydat$boxd <- as.numeric(as.character(dummydat$boxd))
dummydat$covd <- as.numeric(as.character(dummydat$covd))
dummydat$yval <- dummydat$covd

pcov <- ggplot(data = dummydat) +
  geom_bar(aes(x = reorder(yval, rev(yval)), y = as.numeric(boxd), group = 1), fill = "white", col = "black", stat = "identity", position = "dodge") +
  geom_bar(aes(x = reorder(yval, rev(yval)), y = as.numeric(yval), group = yval), stat = "identity", position = "dodge") +
  scale_x_discrete(expand = c(0, 0), breaks = covd, labels = rev(covd)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Coverage (%)", y = "Emerging adult mosquitoes (%)") +
  customTheme_noAngle

pcov <- pcov + labs(title = "A) Coverage per deployment", subtitle = "") + theme(plot.title = element_text(size = 20, vjust = 0.5, hjust = 0))
pdeployDat <- pdeployDat + labs(title = "B) Application rate", subtitle = "") + theme(plot.title = element_text(size = 20, vjust = 0.5, hjust = 0))
pseasonDeploy <- pseasonDeploy + labs(title = "D) Seasonal deployment", subtitle = "") + theme(plot.title = element_text(size = 20, vjust = 0.5, hjust = 0))
pseason <- pseason + labs(title = "C) Seasonality & transmission intensity", subtitle = "") + theme(plot.title = element_text(size = 20, vjust = 0.5, hjust = 0))

pdeploy <- plot_grid(pcov, pdeployDat, rel_widths = c(0.75, 1), nrow = 1)
pseasons <- plot_grid(pseason, pseasonDeploy, rel_widths = c(1, 1), nrow = 1)
methodPlot <- plot_grid(pdeploy, pseasons, nrow = 2)


ggsave("Figure2.png", plot = methodPlot, path = file.path("figures"), width = 14, height = 10, device = "png")
ggsave("Figure2.pdf", plot = methodPlot, path = file.path("figures"), width = 14, height = 10, device = "pdf")
