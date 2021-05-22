## ===================================
## PREPARE SIMULATION OUTPUTS FOR ANALYIS
## Experiment: LSM_SeasonalityTiming_extended
## ===================================
source(file.path("scripts/settings.R"))

### Load continuous data
exp_dir <- file.path("experiments", "LSM_SeasonalityTiming_extended", "processedExpResults")
load(file.path(exp_dir, "rawdat_combined_continuous.Rdata"))
# rawdat_combined <- subset(rawdat_combined, timestep >= 7378 & timestep <= 7817)   ### 2001 to 2006
# load(file.path(ExperimentDataDir,"rawdat_combined_continuous_sub.Rdata"))


## Define variables
timeVARS <- c(
  "timestep", "MonitoringDays", "PostInterventionDates_1", "PostInterventionDates_2", "PostInterventionDates_3",
  "year", "deploy_timestep_2002", "deploy_timestep_2003", "deploy_dates_2002", "deploy_dates_2003"
)
groupVARS <- c("seed", "EIR", "SeasonID", "SeasonID_label", "Duration_mth", "deploy_mth", "Duration_yr", "developmentDuration", "femaleEggsLaidByOviposit", "developmentSurvival")
groupVARS_time <- c(groupVARS, timeVARS)
groupVARSALL <- c(groupVARS, "LARVcov")
groupVARSALL_time <- c(groupVARS_time, "LARVcov")


colnames(rawdat_combined) <- gsub("_", "", gsub("[.]gambiae[.]", "", colnames(rawdat_combined)))
rawdat_combined$SeasonID <- gsub("_", "", rawdat_combined$SeasonID)
rawdat_combined$SeasonID_label <- NA
rawdat_combined$SeasonID_label[rawdat_combined$SeasonID == "0"] <- "No seasonality"
rawdat_combined$SeasonID_label[rawdat_combined$SeasonID == "1-1"] <- "Medium seasonality,\none transmission season"
rawdat_combined$SeasonID_label[rawdat_combined$SeasonID == "1-2"] <- "Medium seasonality,\ntwo transmission seasons"
rawdat_combined$SeasonID_label[rawdat_combined$SeasonID == "2-1"] <- "High seasonality,\none transmission season"
rawdat_combined$SeasonID_label[rawdat_combined$SeasonID == "2-2"] <- "High seasonality,\ntwo transmission seasons"
table(rawdat_combined$SeasonID_label, rawdat_combined$SeasonID, exclude = NULL)


## alpha= The availability rate of humans to mosquitoes (averaged across human population); units: humans/day
numVars <- c(
  "EIR", "LARVcov", "Nv", "Nv0", "seed", "Sv", "Ov", "developmentDuration", "developmentSurvival", "femaleEggsLaidByOviposit",
  "patent.hosts", "new.infections", "res.avail", "alphai", "simulated.EIR", "timestep", "hosts"
)

rawdat_combined[, numVars] <- lapply(rawdat_combined[, numVars], as.numeric)
rawdat_combined$PR <- rawdat_combined$patent.hosts / rawdat_combined$hosts
rawdat_combined$Date <- TimestepToDate(rawdat_combined$timestep, as.Date("1900-01-01"))
rawdat_combined$MonitoringDays <- as.numeric(rawdat_combined$Date - startLSM_date)
rawdat_combined$year <- year(rawdat_combined$Date)

## get start date
## load sweep
load(file.path("dat", "Mth_Duration_TotalDur.Rdata"))
datAll <- datAll[-grep("_1yr", datAll$name), ]

datAll_1 <- datAll %>%
  mutate(MthDurationTotalDur = name, timestep_deploy = as.numeric(as.character(timestep))) %>%
  mutate(year = paste0("deploy_timestep_", year(dates))) %>%
  select(-name, -L, -timestep, -dates) %>%
  arrange(MthDurationTotalDur) %>%
  spread(year, timestep_deploy)

datAll_2 <- datAll %>%
  mutate(MthDurationTotalDur = name, dates_deploy = as.Date(as.character(dates))) %>%
  mutate(year = paste0("deploy_dates_", year(dates))) %>%
  select(-name, -L, -timestep, -dates) %>%
  arrange(MthDurationTotalDur) %>%
  spread(year, dates_deploy) %>%
  merge(datAll_1, by = "MthDurationTotalDur")

rawdat_combinedForMerge <- rawdat_combined
rm(rawdat_combined)
rawdat_combined <- left_join(rawdat_combinedForMerge, datAll_2, by = "MthDurationTotalDur", all.x = TRUE)

rawdat_combined$PostInterventionTimesteps_1 <- as.numeric(rawdat_combined$timestep - rawdat_combined$deploy_timestep_2002)
rawdat_combined$PostInterventionTimesteps_2 <- as.numeric(rawdat_combined$timestep - rawdat_combined$deploy_timestep_2003)

rawdat_combined$PostInterventionDates_1 <- as.numeric(rawdat_combined$Date - rawdat_combined$deploy_dates_2002)
rawdat_combined$PostInterventionDates_2 <- as.numeric(rawdat_combined$Date - rawdat_combined$deploy_dates_2003)
rawdat_combined$PostInterventionDates_3 <- as.numeric(rawdat_combined$deploy_dates_2003 - rawdat_combined$deploy_dates_2002)


summary(rawdat_combined$Date)
tapply(rawdat_combined$MonitoringDays, rawdat_combined$MthDurationTotalDur, summary)
tapply(rawdat_combined$PostInterventionDates_1, rawdat_combined$MthDurationTotalDur, summary)
tapply(rawdat_combined$PostInterventionDates_2, rawdat_combined$MthDurationTotalDur, summary)
tapply(rawdat_combined$PostInterventionDates_3, rawdat_combined$MthDurationTotalDur, summary)


rawdat_combined$MthDurationTotalDur_temp <- rawdat_combined$MthDurationTotalDur
rawdat_combined <- separate(rawdat_combined, MthDurationTotalDur_temp, into = c("deploy_mth", "Duration_mth", "Duration_yr"), sep = "_")
rawdat_combined$Duration_mth <- as.numeric(gsub("mth", "", rawdat_combined$Duration_mth))
rawdat_combined$Duration_yr <- as.numeric(gsub("yr", "", rawdat_combined$Duration_yr))
rawdat_combined$deploy_mth <- as.numeric(rawdat_combined$deploy_mth)


## additional timevars
rawdat_combined$month <- month(rawdat_combined$Date)
rawdat_combined$EffectSeason <- NA
rawdat_combined$EffectSeason[rawdat_combined$deploy_mth %in% c(11, 12, 1, 2, 3)] <- "Most effective"
rawdat_combined$EffectSeason[rawdat_combined$deploy_mth %in% c(4, 5, 6, 7, 8, 9, 10)] <- "Least effective"


## Months relative to peak
rawdat_combined$deploy_mth_adj <- factor(rawdat_combined$deploy_mth,
  levels = c(10:12, 1:9),
  labels = c(-5:6)
)

## Shifted one months back - 1 month lag time between rainfall and vector abundance
rawdat_combined$rainfall_mth <- rawdat_combined$deploy_mth - 1
rawdat_combined$rainfall_mth[rawdat_combined$rainfall_mth == 0] <- 12

rawdat_combined$rainfall_mth_adj <- factor(rawdat_combined$rainfall_mth,
  levels = c(10:12, 1:9),
  labels = c(-5:6)
)


### Start at any time of the season (each 4mths duration)
rawdat_combined$MidOfRain_any <- 0
rawdat_combined$DrySeason_any <- 0
rawdat_combined$BeginofRain_any <- 0
rawdat_combined$EndofRain_any <- 0

rawdat_combined$MidOfRain_any[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-3:0)] <- 1
rawdat_combined$DrySeason_any[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(3:6)] <- 1
rawdat_combined$BeginofRain_any[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-5:-2)] <- 1
rawdat_combined$EndofRain_any[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-1:2)] <- 1

### Start exactly at one months, to cover exactly those 4 months
rawdat_combined$MidOfRain <- 0
rawdat_combined$DrySeason <- 0
rawdat_combined$BeginofRain <- 0
rawdat_combined$EndofRain <- 0
rawdat_combined$ThroughoutTheYear <- 0

rawdat_combined$MidOfRain[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-3)] <- 1
rawdat_combined$DrySeason[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(3)] <- 1
rawdat_combined$BeginofRain[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-5)] <- 1
rawdat_combined$EndofRain[rawdat_combined$Duration_mth != 12 & rawdat_combined$deploy_mth_adj %in% c(-1)] <- 1
rawdat_combined$ThroughoutTheYear[rawdat_combined$Duration_mth == 12] <- 1

table(rawdat_combined$MidOfRain_any, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$DrySeason_any, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$BeginofRain_any, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$EndofRain_any, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$MidOfRain, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$DrySeason, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$BeginofRain, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$EndofRain, rawdat_combined$deploy_mth_adj, exclude = NULL)
table(rawdat_combined$ThroughoutTheYear, rawdat_combined$deploy_mth_adj, exclude = NULL)


### Update grouping vars
timeVARS <- c(
  timeVARS, "month", "EffectSeason", "deploy_mth_adj", "rainfall_mth", "rainfall_mth_adj",
  "MidOfRain_any", "DrySeason_any", "BeginofRain_any", "EndofRain_any",
  "MidOfRain", "DrySeason", "BeginofRain", "EndofRain", "ThroughoutTheYear"
)
groupVARSALL_time <- c(groupVARSALL, timeVARS)


### Calculate relative reduction (per timepoint or compared to baseline-per timepoint)
### group over time
rawdat_combined_red <- as.data.table(rawdat_combined, key = groupVARS_time)
rawdat_combined_red[, red_Nv := ((Nv[LARVcov == 0] - Nv) / Nv[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_Nv0 := ((Nv0[LARVcov == 0] - Nv0) / Nv0[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_new.infections := ((new.infections[LARVcov == 0] - new.infections) / new.infections[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_PR := ((PR[LARVcov == 0] - PR) / PR[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_simulated.EIR := ((simulated.EIR[LARVcov == 0] - simulated.EIR) / simulated.EIR[LARVcov == 0]) * 100, by = groupVARS_time]

### group regardless of time
rawdat_combined_red <- as.data.table(rawdat_combined_red, key = groupVARS)
rawdat_combined_red[, redBase_Nv := ((Nv[MonitoringDays == 0] - Nv) / Nv[MonitoringDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_Nv0 := ((Nv0[MonitoringDays == 0] - Nv0) / Nv0[MonitoringDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_new.infections := ((new.infections[MonitoringDays == 0] - new.infections) / new.infections[MonitoringDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_PR := ((PR[MonitoringDays == 0] - PR) / PR[MonitoringDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_simulated.EIR := ((simulated.EIR[MonitoringDays == 0] - simulated.EIR) / simulated.EIR[MonitoringDays == 0]) * 100, by = groupVARS]

## -------------------------------------------
## Summary statistics and save
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$Duration_mth, summary)
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$LARVcov, summary)
tapply(rawdat_combined_red$redBase_Nv0, rawdat_combined_red$LARVcov, summary)

save(rawdat_combined_red, file = file.path(exp_dir, "rawdat_combined_red.Rdata"))
# load(file.path(exp_dir, "rawdat_combined_red.Rdata"))


## -------------------------------------------
#### Cumulative
rawdat_combined_cum <- rawdat_combined_red %>%
  dplyr::group_by_(groupVARSALL_time) %>%
  dplyr::filter(year <= 2008) %>%
  dplyr::arrange(timestep) %>%
  dplyr::mutate(
    red_simulated.EIR_cum = cumsum(red_simulated.EIR),
    red_PR_cum = cumsum(red_PR),
    red_Nv0_cum = cumsum(red_Nv0),
    red_Nv_cum = cumsum(red_Nv),
    red_simulated.EIR_cummean = cummean(red_simulated.EIR),
    red_PR_cummean = cummean(red_PR),
    red_new.infections_cummean = cummean(red_new.infections),
    red_Nv0_cummean = cummean(red_Nv0),
    red_Nv_cummean = cummean(red_Nv),
    redBase_simulated.EIR_cum = cumsum(redBase_simulated.EIR),
    redBase_PR_cum = cumsum(redBase_PR),
    redBase_Nv0_cum = cumsum(redBase_Nv0),
    redBase_Nv_cum = cumsum(redBase_Nv),
    redBase_simulated.EIR_cummean = cummean(redBase_simulated.EIR),
    redBase_PR_cummean = cummean(redBase_PR),
    redBase_new.infections_cummean = cummean(redBase_new.infections),
    redBase_Nv0_cummean = cummean(redBase_Nv0),
    redBase_Nv_cummean = cummean(redBase_Nv)
  ) %>%
  as.data.frame()


## -------------------------------------------
## Summary statistics and save
tapply(rawdat_combined_cum$red_Nv0_cum, rawdat_combined_cum$Duration_mth, summary)
tapply(rawdat_combined_cum$red_Nv0_cum, rawdat_combined_cum$LARVcov, summary)
tapply(rawdat_combined_cum$red_Nv0_cummean, rawdat_combined_cum$Duration_mth, summary)
tapply(rawdat_combined_cum$red_Nv0_cummean, rawdat_combined_cum$LARVcov, summary)
save(rawdat_combined_cum, file = file.path(exp_dir, "rawdat_combined_cum.Rdata"))
