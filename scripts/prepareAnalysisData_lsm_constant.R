## ===================================
## PREPARE SIMULATION OUTPUTS FOR ANALYIS
## Experiment: LSM_constant
## ===================================
source(file.path("scripts/settings.R"))

### Load continuous data
exp_dir <- file.path("experiments", "LSM_constant", "processedExpResults")
load(file.path(exp_dir, "rawdat_combined_continuous.Rdata"))

## -------------------------------------------
#selectedDecay <- "Decay40at10days"
selectedDecay="Decay5daystep"
## -------------------------------------------


## Define variables
timeVARS <- c("timestep", "PostInterventionDays", "year")
groupVARS <- c("seed", "Decay2", "EIR", "Frequency", "Duration", "developmentDuration", "femaleEggsLaidByOviposit", "developmentSurvival")
groupVARS_time <- c(groupVARS, timeVARS)
groupVARSALL <- c(groupVARS, "LARVcov")
groupVARSALL_time <- c(groupVARS_time, "LARVcov")

## Subset and rename column names
rawdat_combined <- rawdat_combined %>% filter(timestep >= 7432 & timestep <= 7552)
colnames(rawdat_combined) <- gsub("_", "", gsub("[.]gambiae[.]", "", colnames(rawdat_combined)))

## alpha= The availability rate of humans to mosquitoes (averaged across human population); units: humans/day
numVars <- c(
  "EIR", "LARVcov", "Nv", "Nv0", "seed", "Sv", "Ov", "developmentDuration", "developmentSurvival", "femaleEggsLaidByOviposit",
  "patent.hosts", "new.infections", "res.avail", "alphai", "simulated.EIR", "timestep", "hosts"
)
rawdat_combined[, numVars] <- lapply(rawdat_combined[, numVars], as.numeric)

rawdat_combined$PR <- rawdat_combined$patent.hosts / rawdat_combined$hosts
rawdat_combined$PostInterventionTimesteps <- rawdat_combined$timestep - startLSM
rawdat_combined$Date <- TimestepToDate(rawdat_combined$timestep, as.Date("1900-01-01"))
rawdat_combined$PostInterventionDays <- as.numeric(rawdat_combined$Date - startLSM_date)
rawdat_combined$year <- year(rawdat_combined$Date)

summary(rawdat_combined$Date)
summary(rawdat_combined$PostInterventionDays)

### remove timesteps not needed (to save memory)
rawdat_combined <- rawdat_combined %>%
  filter(
    PostInterventionDays >= -10,
    LARVcov %in% c(0, 20, 40, 60, 80, 100))

### ----------------------------------------------------
rawdat_combined$LSMDeployTime_temp <- rawdat_combined$LSMDeployTime
table(rawdat_combined$LSMDeployTime_temp)

rawdat_combined <- separate(rawdat_combined, LSMDeployTime_temp, into = c("Duration", "Frequency"), sep = "_")
rawdat_combined$Duration <- as.numeric(gsub("timedat", "", rawdat_combined$Duration))
rawdat_combined$Frequency <- as.numeric(gsub("timesteps", "", rawdat_combined$Frequency))
rawdat_combined$Frequency[is.na(rawdat_combined$Frequency)] <- "once"

### group over time
rawdat_combined_red <- as.data.table(rawdat_combined, key = groupVARS_time)
rawdat_combined_red[, red_Nv := ((Nv[LARVcov == 0] - Nv) / Nv[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_Nv0 := ((Nv0[LARVcov == 0] - Nv0) / Nv0[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_new.infections := ((new.infections[LARVcov == 0] - new.infections) / new.infections[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_PR := ((PR[LARVcov == 0] - PR) / PR[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_simulated.EIR := ((simulated.EIR[LARVcov == 0] - simulated.EIR) / simulated.EIR[LARVcov == 0]) * 100, by = groupVARS_time]

### group regardless of time
rawdat_combined_red <- as.data.table(rawdat_combined_red, key = groupVARS)
rawdat_combined_red[, redBase_Nv := ((Nv[PostInterventionDays == 0] - Nv) / Nv[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_Nv0 := ((Nv0[PostInterventionDays == 0] - Nv0) / Nv0[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_new.infections := ((new.infections[PostInterventionDays == 0] - new.infections) / new.infections[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_PR := ((PR[PostInterventionDays == 0] - PR) / PR[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_simulated.EIR := ((simulated.EIR[PostInterventionDays == 0] - simulated.EIR) / simulated.EIR[PostInterventionDays == 0]) * 100, by = groupVARS]

### Summary statistics
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$Duration, summary)
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$LARVcov, summary)
tapply(rawdat_combined_red$redBase_Nv0, rawdat_combined_red$LARVcov, summary)
# save(rawdat_combined_red, file=file.path(exp_dir, "rawdat_combined_red.Rdata"))
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
head(rawdat_combined_cum)

tapply(rawdat_combined_cum$red_Nv0_cum, rawdat_combined_cum$Duration, summary)
tapply(rawdat_combined_cum$red_Nv0_cum, rawdat_combined_cum$LARVcov, summary)
tapply(rawdat_combined_cum$red_Nv0_cummean, rawdat_combined_cum$Duration, summary)
tapply(rawdat_combined_cum$red_Nv0_cummean, rawdat_combined_cum$LARVcov, summary)

### long format  & aggregate seeds & densitiy dependend parameter
rm(rawdat_combined, rawdat_combined_red)
outcomeVars <- c(
  "red_simulated.EIR_cummean", "red_PR_cummean", "red_Nv0_cummean",
  "red_simulated.EIR_cummean", "red_PR_cummean", "red_Nv0_cummean",
  "red_simulated.EIR_cummean", "red_PR_cummean",
  "red_Nv0_cummean", "red_simulated.EIR_cummean", "red_PR_cummean",
  "red_Nv0_cummean", "red_simulated.EIR", "red_PR", "red_Nv0",
  "red_simulated.EIR", "red_PR", "red_Nv0",
  "redBase_simulated.EIR_cum",
  "redBase_PR_cum",
  "redBase_Nv0_cum",
  "redBase_Nv_cum",
  "redBase_simulated.EIR_cummean",
  "redBase_PR_cummean",
  "redBase_new.infections_cummean",
  "redBase_Nv0_cummean",
  "redBase_Nv_cummean"
)


### wide to long format
rawdat_combined_cum_long <- rawdat_combined_cum %>%
  mutate(
    simulatedEIR__cummean = red_simulated.EIR_cummean,
    PR__cummean = red_PR_cummean,
    Nv0__cummean = red_Nv0_cummean,
    Nv__cummean = red_Nv_cummean,
    simulatedEIR__cum = red_simulated.EIR_cum,
    PR__cum = red_PR_cum,
    Nv0__cum = red_Nv0_cum,
    simulatedEIR__red = red_simulated.EIR,
    PR__red = red_PR,
    Nv0__red = red_Nv0,
    Nv__red = red_Nv,
    PR__redBase = red_PR,
    Nv0__redBase = redBase_Nv0,
    simulatedEIR__cummeanBase = redBase_simulated.EIR_cummean,
    PR__cummeanBase = redBase_PR_cummean,
    Nv0__cummeanBase = redBase_Nv0_cummean,
    simulatedEIR__cumBase = redBase_simulated.EIR_cum,
    PR__cumBase = redBase_PR_cum,
    Nv0__cumBase = redBase_Nv0_cum,
    Nv__cumBase = redBase_Nv_cum,
    simulatedEIR__redBase = redBase_simulated.EIR,
    PR__redBase = redBase_PR,
    Nv0__redBase = redBase_Nv0,
    Nv__redBase = redBase_Nv
  ) %>%
  dplyr::select(
    EIR, Decay2, Duration, Frequency, LARVcov, timestep, year, PostInterventionDays, seed, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival,
    simulatedEIR__cummean,
    PR__cummean,
    Nv0__cummean,
    Nv__cummean,
    simulatedEIR__cum,
    PR__cum,
    Nv0__cum,
    simulatedEIR__red,
    PR__red,
    Nv0__red,
    Nv__red,
    PR__redBase,
    Nv0__redBase,
    simulatedEIR__cummeanBase,
    PR__cummeanBase,
    Nv0__cummeanBase,
    simulatedEIR__cumBase,
    PR__cumBase,
    Nv0__cumBase,
    Nv__cumBase,
    simulatedEIR__redBase,
    PR__redBase,
    Nv0__redBase,
    Nv__redBase
  ) %>%
  as.data.frame()


rawdat_combined_cum_long <- rawdat_combined_cum_long %>%
  filter(Decay2 == "decay_5dayStep") %>%
  dplyr::select(
    -c(
      simulatedEIR__cummean,
      PR__cummean,
      Nv0__cummean,
      Nv__cummean
    ),
    simulatedEIR__cummeanBase,
    PR__cummeanBase,
    Nv0__cummeanBase
  )

idVARS <- groupVARSALL_time
rawdat_combined_cum_long <- as.data.table(rawdat_combined_cum_long, key = idVARS)
rawdat_combined_cum_long <- melt.data.table(rawdat_combined_cum_long, id = idVARS)

rawdat_combined_cum_long <- separate(rawdat_combined_cum_long, variable, into = c("outcome", "cum"), sep = "__")
head(rawdat_combined_cum_long)
summary(rawdat_combined_cum_long$value)
table(rawdat_combined_cum_long$outcome, rawdat_combined_cum_long$cum)

table(rawdat_combined_cum_long$outcome)
rawdat_combined_cum_long$outcomeLabel <- rawdat_combined_cum_long$outcome
rawdat_combined_cum_long$outcomeLabel <- gsub("Nv$", "Host-seeking mosquitoes", rawdat_combined_cum_long$outcomeLabel)
rawdat_combined_cum_long$outcomeLabel <- gsub("Nv0", "Mosquito emergence", rawdat_combined_cum_long$outcomeLabel)
rawdat_combined_cum_long$outcomeLabel <- gsub("simulatedEIR", "EIR", rawdat_combined_cum_long$outcomeLabel)
rawdat_combined_cum_long$outcomeLabel <- gsub("PR", "Prevalence", rawdat_combined_cum_long$outcomeLabel)
rawdat_combined_cum_long$outcomeLabel <- factor(rawdat_combined_cum_long$outcomeLabel,
  levels = c("Mosquito emergence", "Host-seeking mosquitoes", "EIR", "Prevalence"),
  labels = c("Mosquito emergence", "Host-seeking mosquitoes", "EIR", "Prevalence")
)
table(rawdat_combined_cum_long$outcomeLabel, exclude = NULL)
table(rawdat_combined_cum_long$outcomeLabel, rawdat_combined_cum_long$outcome, exclude = NULL)

rawdat_combined_cum_long$Frequency_fct <- factor(rawdat_combined_cum_long$Frequency,
  levels = c("1", "2", "3", "4", "5", "6", "12", "once"),
  labels = c("5", "10", "15", "20", "25", "30", "60", "once")
)
table(rawdat_combined_cum_long$Frequency_fct, rawdat_combined_cum_long$Frequency, exclude = NULL)

## Save dataset
save(rawdat_combined_cum_long, file = file.path(exp_dir, paste0("DataForAnalysis_", selectedDecay, ".Rdata")))
