## ===================================
## PREPARE SIMULATION OUTPUTS FOR ANALYIS
## Experiment: LSM_constant_duration
## ===================================
source(file.path("scripts/settings.R"))

## Define variables
timeVARS <- c("timestep", "PostInterventionDays", "year")
groupVARS <- c("seed", "EIR", "Duration", "developmentDuration", "femaleEggsLaidByOviposit", "developmentSurvival")
groupVARS_time <- c(groupVARS, timeVARS)
groupVARSALL <- c(groupVARS, "LARVcov")
groupVARSALL_time <- c(groupVARS_time, "LARVcov")
#### ======================================================


### Load continuous data
exp_dir <- file.path("experiments", "LSM_constant_duration", "processedExpResults")
load(file.path(exp_dir, "rawdat_combined_continuous.Rdata"))

rawdat_combined <- rawdat_combined %>% filter(PostInterventionDays >= -10 & PostInterventionDays <= 500)
# PostInterventionDays == 365
### ----------------------------------------------------
rawdat_combined$LSMDeployTime_temp <- rawdat_combined$LSMDeployTime
table(rawdat_combined$LSMDeployTime_temp)

rawdat_combined <- separate(rawdat_combined, LSMDeployTime_temp, into = c("LSM", "Season", "Frequency", "Duration"), sep = "_")
rawdat_combined$Duration <- as.numeric(gsub("mth", "", rawdat_combined$Duration))

table(rawdat_combined$Duration, exclude = NULL)
table(rawdat_combined$Frequency, exclude = NULL) ## constant frequency
table(rawdat_combined$Season, exclude = NULL) ## timing does not matter if having no seasonality

rawdat_combined <- rawdat_combined %>% filter(Duration %in% c(1, 2, 3, 4, 12)) ## subset due to memory issues

##### Counterfactual
rawdat_combined_red <- as.data.table(rawdat_combined, key = groupVARS_time)
rm(rawdat_combined)

rawdat_combined_red[, red_Nv := ((Nv[LARVcov == 0] - Nv) / Nv[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_Nv0 := ((Nv0[LARVcov == 0] - Nv0) / Nv0[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_new.infections := ((new.infections[LARVcov == 0] - new.infections) / new.infections[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_PR := ((PR[LARVcov == 0] - PR) / PR[LARVcov == 0]) * 100, by = groupVARS_time]
rawdat_combined_red[, red_simulated.EIR := ((simulated.EIR[LARVcov == 0] - simulated.EIR) / simulated.EIR[LARVcov == 0]) * 100, by = groupVARS_time]

#### Without time variables
rawdat_combined_red <- as.data.table(rawdat_combined_red, key = groupVARS)
rawdat_combined_red[, redBase_Nv := ((Nv[PostInterventionDays == 0] - Nv) / Nv[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_Nv0 := ((Nv0[PostInterventionDays == 0] - Nv0) / Nv0[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_new.infections := ((new.infections[PostInterventionDays == 0] - new.infections) / new.infections[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_PR := ((PR[PostInterventionDays == 0] - PR) / PR[PostInterventionDays == 0]) * 100, by = groupVARS]
rawdat_combined_red[, redBase_simulated.EIR := ((simulated.EIR[PostInterventionDays == 0] - simulated.EIR) / simulated.EIR[PostInterventionDays == 0]) * 100, by = groupVARS]

### Summary stats of alculated reduction variables
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$Duration, summary)
tapply(rawdat_combined_red$red_Nv0, rawdat_combined_red$LARVcov, summary)
tapply(rawdat_combined_red$redBase_Nv0, rawdat_combined_red$LARVcov, summary)


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
save(rawdat_combined_cum, rawdat_combined_red, file = file.path(exp_dir, "rawdat_combined_cum.Rdata"))

### long format  & aggregate seeds & densitiy dependend parameter
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
    EIR, Duration, LARVcov, timestep, year, PostInterventionDays, seed, developmentDuration, femaleEggsLaidByOviposit, developmentSurvival,
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

idVARS <- groupVARSALL_time
rawdat_combined_cum_long <- as.data.table(rawdat_combined_cum_long, key = idVARS)
rawdat_combined_cum_long <- melt.data.table(rawdat_combined_cum_long, id = idVARS)
rawdat_combined_cum_long <- separate(rawdat_combined_cum_long, variable, into = c("outcome", "cum"), sep = "__")
head(rawdat_combined_cum_long)
summary(rawdat_combined_cum_long$value)

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

### Save dataset
save(rawdat_combined_cum_long, file = file.path(exp_dir, "DataForAnalysis.Rdata"))
