

#### Packages and custom Settings
source(file.path("scripts/settings.R"))


SweepDir <- file.path("deployment_times", "LSMDeployTime")
if (!dir.exists(SweepDir)) dir.create(SweepDir)


## ===========DATE TO TIMESTEP FUNCTION ============
dateToTimestep <- function(datevar, SIMSTART, digits = 1, miss = NULL, label = FALSE, print = TRUE) {
  out <- as.numeric(round((datevar - SIMSTART) / 5 + 1, 0))
  return(out)
}
TimestepToDate <- function(timesteps, SIMSTART, digits = 1, miss = NULL, label = FALSE, print = TRUE) {
  out <- as.Date(timesteps * 5, origin = SIMSTART)
  return(out)
}


## ===========SIMULATION START===================
start <- as.Date("1900-01-01")
monitoringStart <- as.Date("2000-01-01")
monitoringEnd <- as.Date("2011-01-01")
## ==============================================


## ==============================================
##### MONITORING TIME
## ==============================================
MonitoringTime <- seq(monitoringStart, monitoringEnd, "days")
timesteps <- dateToTimestep(MonitoringTime, start)
timesteps <- unique(timesteps)
Surveys <- cbind(as.data.frame(timesteps), as.data.frame(TimestepToDate(timesteps, start)))
names(Surveys) <- c("timesteps", "date")


#### Export xml
sink(file.path(SweepDir, "Survey_Timesteps.txt"), append = FALSE)
cat(paste0("<surveyTime>", Surveys[, 1], "</surveyTime> <!--", Surveys[, 2], "-->", "\n"))
sink()


## ==============================================
##### SEASONALITY
## ==============================================
### Reproduced  from SI TEXT 1 from
### 1. Stuckey EM, Smith T, Chitnis N. Seasonally Dependent Relationships between Indicators of Malaria
### Transmission and  Disease Provided by Mathematical Model Simulations.
### PLOS Computational Biology. 2014 Sep 4;10(9):e1003812.
lessStrong_group7_xml <- c(0.02, 0.03, 0.05, 0.07, 0.13, 0.14, 0.06, 0.04, 0.03, 0.03, 0.01, 0.02)
strong_group6_xml <- c(0.00, 0.00, 0.04, 0.07, 0.18, 0.22, 0.09, 0.02, 0.00, 0.00, 0.00, 0.00)

## rainfall - 1 months lag
lessStrong_group7 <- c(0.03, 0.05, 0.07, 0.13, 0.14, 0.06, 0.04, 0.03, 0.03, 0.01, 0.02, 0.02)
strong_group6 <- c(0.00, 0.04, 0.07, 0.18, 0.22, 0.09, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00)

seasonDat <- as.data.frame(cbind(c(1:12), lessStrong_group7_xml, strong_group6_xml, lessStrong_group7, strong_group6))

### Export for xml
sink(file.path(SweepDir, "lessStrong_group7.txt"), append = FALSE)
cat(paste0("<value>", lessStrong_group7_xml, "</value>", "\n"))
sink()

sink(file.path(SweepDir, "strong_group6.txt"), append = FALSE)
cat(paste0("<value>", strong_group6_xml, "</value>", "\n"))
sink()


#### Buld seasonalityDat and plot
lessStrong_group7_xml <- as.data.frame(cbind(lessStrong_group7_xml, "lessStrong_group7_xml"))
strong_group6_xml <- as.data.frame(cbind(strong_group6_xml, "strong_group6_xml"))

names(lessStrong_group7_xml) <- c("seasonality", "seasonalityName")
names(strong_group6_xml) <- c("seasonality", "seasonalityName")

strong_group6_xml$month <- as.numeric(rownames(strong_group6_xml))
lessStrong_group7_xml$month <- as.numeric(rownames(lessStrong_group7_xml))

strong_group6_xml$seasonality <- as.numeric(as.character(strong_group6_xml$seasonality))
lessStrong_group7_xml$seasonality <- as.numeric(as.character(lessStrong_group7_xml$seasonality))

seasonalityDat <- rbind(lessStrong_group7_xml, strong_group6_xml)

seasonalityDat$month_rainfall <- seasonalityDat$month - 1
seasonalityDat$month_rainfall[seasonalityDat$month_rainfall == 0] <- 12

ggplot(data = seasonalityDat) +
  geom_line(aes(x = month, y = seasonality, col = seasonalityName, group = seasonalityName), size = 1.7) +
  labs(title = "transmission (xml)")
ggplot(data = seasonalityDat) +
  geom_line(aes(x = month_rainfall, y = seasonality, col = seasonalityName, group = seasonalityName), size = 1.7) +
  labs(title = "rainfall")


## ==============================================
##### DEFINE SEASONS
## ==============================================
seasonalityDat$WetSeason <- NA
seasonalityDat$DrySeason <- NA
seasonalityDat$EndOfDrySeason <- NA
seasonalityDat$WetSeason[seasonalityDat$month_rainfall %in% c(3, 4, 5, 6)] <- 1
seasonalityDat$DrySeason[seasonalityDat$month_rainfall %in% c(9, 10, 11, 12)] <- 1
seasonalityDat$EndOfDrySeason[seasonalityDat$month_rainfall %in% c(1, 2, 3, 4)] <- 1

seasonalityDat$year <- 2002
seasonalityDat2 <- seasonalityDat
seasonalityDat2$year <- 2003
seasonalityDat <- rbind(seasonalityDat, seasonalityDat2)

seasonalityDat$Date_rainfall <- as.Date(paste0(seasonalityDat$year, "-", seasonalityDat$month_rainfall, "-01"))
seasonalityDat$Date <- as.Date(paste0(seasonalityDat$year, "-", seasonalityDat$month, "-01"))

seasonalityDat_long <- pivot_longer(seasonalityDat, cols = -c("seasonality", "seasonalityName", "month", "month_rainfall", "year", "Date_rainfall", "Date"), names_to = "variable")

seasonalityDat_long$value[seasonalityDat_long$variable == "WetSeason"] <- seasonalityDat_long$value[seasonalityDat_long$variable == "WetSeason"] / 9
seasonalityDat_long$value[seasonalityDat_long$variable == "DrySeason"] <- seasonalityDat_long$value[seasonalityDat_long$variable == "DrySeason"] / 8
seasonalityDat_long$value[seasonalityDat_long$variable == "EndOfDrySeason"] <- seasonalityDat_long$value[seasonalityDat_long$variable == "EndOfDrySeason"] / 7

seasonalityDat_long$season_corrected <- as.character(seasonalityDat_long$variable)
seasonalityDat_long$season_corrected[seasonalityDat_long$variable == "WetSeason"] <- "Mid of rain season"
seasonalityDat_long$season_corrected[seasonalityDat_long$variable == "DrySeason"] <- "Dry season"
seasonalityDat_long$season_corrected[seasonalityDat_long$variable == "EndOfDrySeason"] <- "Begin of rain season"

seasonalityDat_longDUMMY <- subset(seasonalityDat_long, variable == "WetSeason")
seasonalityDat_longDUMMY$variable <- "Throughout the year"
seasonalityDat_longDUMMY$value <- 0.2
seasonalityDat_longDUMMY$season_corrected <- "Throughout the year"
seasonalityDat_long <- as.data.frame(rbind(seasonalityDat_long, seasonalityDat_longDUMMY))


## WET SEASON  - MARCH TO END OF JUNE
WETSEASON_yr1 <- seq(as.Date("2002-03-01"), as.Date("2002-07-01"), "days")
WETSEASON_yr2 <- seq(as.Date("2003-03-01"), as.Date("2003-07-01"), "days")
WETSEASON_yr3 <- seq(as.Date("2004-03-01"), as.Date("2004-07-01"), "days")
WETSEASON <- c(WETSEASON_yr1, WETSEASON_yr2, WETSEASON_yr3)

## DRY SEASON  - SEPTEMBER TO END OF DECEMBER
DRYSEASON_yr1 <- seq(as.Date("2002-09-01"), as.Date("2003-01-01"), "days")
DRYSEASON_yr2 <- seq(as.Date("2003-09-01"), as.Date("2004-01-01"), "days")
DRYSEASON_yr3 <- seq(as.Date("2004-09-01"), as.Date("2005-01-01"), "days")
DRYSEASON <- c(DRYSEASON_yr1, DRYSEASON_yr2, DRYSEASON_yr3)

## ENDOFDRY SEASON  - JANUARY TO END OF APRIL
ENDDRYSEASON_yr1 <- seq(as.Date("2002-01-01"), as.Date("2002-05-01"), "days")
ENDDRYSEASON_yr2 <- seq(as.Date("2003-01-01"), as.Date("2003-05-01"), "days")
ENDDRYSEASON_yr3 <- seq(as.Date("2004-01-01"), as.Date("2004-05-01"), "days")
ENDDRYSEASON <- c(ENDDRYSEASON_yr1, ENDDRYSEASON_yr2, ENDDRYSEASON_yr3)

timesteps <- dateToTimestep(WETSEASON, start)
timesteps <- unique(timesteps)
WETSEASON <- cbind(as.data.frame(timesteps), as.data.frame(TimestepToDate(timesteps, start)))
names(WETSEASON) <- c("timesteps", "date")

timesteps <- dateToTimestep(DRYSEASON, start)
timesteps <- unique(timesteps)
DRYSEASON <- cbind(as.data.frame(timesteps), as.data.frame(TimestepToDate(timesteps, start)))
names(DRYSEASON) <- c("timesteps", "date")

timesteps <- dateToTimestep(ENDDRYSEASON, start)
timesteps <- unique(timesteps)
ENDDRYSEASON <- cbind(as.data.frame(timesteps), as.data.frame(TimestepToDate(timesteps, start)))
names(ENDDRYSEASON) <- c("timesteps", "date")


## ==============================================
##### DEFINE DECAY AND WRITE SWEEPS
## ==============================================

###### STEP FUNCTION CONSTANT IMPACT FOR 1,2,3,4, 12, 24 months
decay1mth <- 30 / 365
decay2mth <- 60 / 365
decay3mth <- 90 / 365
decay4mth <- 120 / 365

decayDat <- as.data.frame(rbind(decay1mth, decay2mth, decay3mth, decay4mth))
decayDat$decayName <- rownames(decayDat)
rownames(decayDat) <- c(1:dim(decayDat)[1])
names(decayDat) <- c("L", "decayName")

#### Deployment once per season at different decays and effectiveness durations
deploy3_WETSEASON <- WETSEASON %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()
deploy3_DRYSEASON <- DRYSEASON %>%
  mutate(year = year(date), months = months(date)) %>%
  filter(months == "Oktober") %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()
deploy3_ENDDRYSEASON <- ENDDRYSEASON %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()

deploy2_WETSEASON <- deploy3_WETSEASON[1:2, ]
deploy2_DRYSEASON <- deploy3_DRYSEASON[1:2, ]
deploy2_ENDDRYSEASON <- deploy3_ENDDRYSEASON[1:2, ]

deploy1_WETSEASON <- deploy3_WETSEASON[1, ]
deploy1_DRYSEASON <- deploy3_DRYSEASON[1, ]
deploy1_ENDDRYSEASON <- deploy3_ENDDRYSEASON[1, ]


dataList <- list(
  "LSM_Wet_3yrs" = deploy3_WETSEASON,
  "LSM_Dry_3yrs" = deploy3_DRYSEASON,
  "LSM_endOfdryPartWet_3yrs" = deploy3_ENDDRYSEASON,
  "LSM_Wet_2yrs" = deploy2_WETSEASON,
  "LSM_Dry_2yrs" = deploy2_DRYSEASON,
  "LSM_endOfdryPartWet_2yrs" = deploy2_ENDDRYSEASON,
  "LSM_Wet_1yrs" = deploy1_WETSEASON,
  "LSM_Dry_1yrs" = deploy1_DRYSEASON,
  "LSM_endOfdryPartWet_1yrs" = deploy1_ENDDRYSEASON
)


for (selectedDecayName in decayDat$decayName) {
  # selectedDecayName <-    decayDat$decayName[1]

  L <- decayDat$L[decayDat$decayName == selectedDecayName]

  #### Create txt sweeps
  for (DATname in names(dataList)) {
    # DATname <- names(dataList)[1]

    tempdat <- dataList[[DATname]]

    SWEEPNAMES <- paste0(DATname, "_", gsub("decay", "", selectedDecayName))
    unlink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), recursive = FALSE, force = FALSE)

    sink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), append = FALSE)
    cat(paste0("@L@:", L, "\n", "@deployTime@:"))
    for (i in c(1:dim(tempdat)[1])) {
      cat(paste0('<deploy time="', tempdat[i, 1], '"/>'))
    }
    sink()

    rm(tempdat)
  }
}

rm(dataList)


## ===========================================
#### If doing for 4 months, how much does the duration matter?
### Monthly
deployMonthly_WETSEASON <- WETSEASON %>%
  mutate(Month_Yr = format(as.Date(date), "%Y-%m")) %>%
  group_by(Month_Yr) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()
deployMonthly_DRYSEASON <- DRYSEASON %>%
  mutate(Month_Yr = format(as.Date(date), "%Y-%m")) %>%
  group_by(Month_Yr) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()
deployMonthly_ENDDRYSEASON <- ENDDRYSEASON %>%
  mutate(Month_Yr = format(as.Date(date), "%Y-%m")) %>%
  group_by(Month_Yr) %>%
  slice(1) %>%
  ungroup() %>%
  select(timesteps, date) %>%
  as.data.frame()

dataList <- list(
  "LSM_Wet_monthly" = deployMonthly_WETSEASON,
  "LSM_Dry_monthly" = deployMonthly_DRYSEASON,
  "LSM_endOfdryPartWet_monthly" = deployMonthly_ENDDRYSEASON
)


decay5day <- 5 / 365 # every 1 timestep
decay10day <- 10 / 365 # every 2 timestep
decay15day <- 15 / 365 # every 3 timestep
decay20day <- 20 / 365 # every 4 timestep
decay25day <- 25 / 365 # every 5 timestep
decay30day <- 30 / 365 # every 6 timestep

decayDat <- as.data.frame(rbind(decay5day, decay10day, decay15day, decay20day, decay25day, decay30day))
decayDat$decayName <- rownames(decayDat)
rownames(decayDat) <- c(1:dim(decayDat)[1])
names(decayDat) <- c("L", "decayName")

for (selectedDecayName in decayDat$decayName) {
  # selectedDecayName <-    decayDat$decayName[1]

  L <- decayDat$L[decayDat$decayName == selectedDecayName]

  #### Create txt sweeps
  for (DATname in names(dataList)) {
    # DATname <- names(dataList)[1]

    tempdat <- dataList[[DATname]]

    SWEEPNAMES <- paste0(DATname, "_", gsub("decay", "", selectedDecayName))
    unlink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), recursive = FALSE, force = FALSE)

    sink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), append = FALSE)
    cat(paste0("@L@:", L, "\n", "@deployTime@:"))
    for (i in c(1:dim(tempdat)[1])) {
      cat(paste0('<deploy time="', tempdat[i, 1], '"/>'))
    }
    sink()

    rm(tempdat)
  }
}


## ===========================================
#### only 1 year, 2 years or 3 years
decay12mth <- 1
decay24mth <- 2
decay36mth <- 3

decayDat <- as.data.frame(rbind(decay12mth, decay24mth, decay36mth))
decayDat$decayName <- rownames(decayDat)
rownames(decayDat) <- c(1:dim(decayDat)[1])
names(decayDat) <- c("L", "decayName")

dataList_v1 <- list(
  "LSM_Wet_1yrs" = deploy1_WETSEASON,
  "LSM_Dry_1yrs" = deploy1_DRYSEASON,
  "LSM_endOfdryPartWet_1yrs" = deploy1_ENDDRYSEASON
)


for (selectedDecayName in decayDat$decayName) {
  # selectedDecayName <-    decayDat$decayName[1]

  L <- decayDat$L[decayDat$decayName == selectedDecayName]

  #### Create txt sweeps
  for (DATname in names(dataList_v1)) {
    # DATname <- names(dataList_v1)[1]

    tempdat <- dataList_v1[[DATname]]

    SWEEPNAMES <- paste0(DATname, "_", gsub("decay", "", selectedDecayName))
    unlink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), recursive = FALSE, force = FALSE)

    sink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), append = FALSE)
    cat(paste0("@L@:", L, "\n", "@deployTime@:"))
    for (i in c(1:dim(tempdat)[1])) {
      cat(paste0('<deploy time="', tempdat[i, 1], '"/>'))
    }
    sink()

    rm(tempdat)
  }
}


## ===========================================
###  DIfferent gaps between deployments
datevar1 <- seq(as.Date("2002-01-01"), as.Date("2002-05-01"), "days")
timesteps1 <- dateToTimestep(datevar1, start)
unitimesteps12 <- unique(timesteps1)

## every1timesteps =~ every  ~5 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 1)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_1timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## every2timesteps =~ every week ~10 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 2)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_2timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## every3timesteps =~ every 2 weeks  ~15 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 3)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_3timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## every4timesteps =~ every 3 weeks  ~20 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 4)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_4timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## every5timesteps =~ every 3.5 weeks  ~25 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 5)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_5timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))


## every6timesteps =~ every months -  4 weeks  ~30 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 6)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_6timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## every12timesteps =~ every 2 months -  8 weeks  ~30 days
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[seq(1, length(unitimesteps12), 12)]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_12timesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))

## everyFirsttimesteps =~ once at the beginning
rm(Dates, Timesteps)
Timesteps <- unitimesteps12[1]
Dates <- TimestepToDate(Timesteps, start)
(timedat12_firsttimesteps <- as.data.frame(cbind(as.data.frame(Timesteps), as.data.frame(Dates))))


dataList_v2 <- list(
  "timedat12_12timesteps" = timedat12_12timesteps,
  "timedat12_firsttimesteps" = timedat12_firsttimesteps,
  "timedat12_6timesteps" = timedat12_6timesteps,
  "timedat12_5timesteps" = timedat12_5timesteps,
  "timedat12_4timesteps" = timedat12_4timesteps,
  "timedat12_3timesteps" = timedat12_3timesteps,
  "timedat12_2timesteps" = timedat12_2timesteps,
  "timedat12_1timesteps" = timedat12_1timesteps
)


### Write names of txt sweeps into object
(SweepNames <- names(dataList_v2))


L <- 5 / 365 ## decay 5 days
#### Create txt sweeps
for (DATname in names(dataList_v2)) {
  # DATname <- names(dataList_v1)[1]

  tempdat <- dataList_v2[[DATname]]

  SWEEPNAMES <- paste0(DATname)

  unlink(file.path(SweepDir, paste0(SWEEPNAMES, ".txt")), recursive = FALSE, force = FALSE)

  sink(file.path(SweepDir,paste0(SWEEPNAMES, ".txt")), append = FALSE)
  cat(paste0("@L@:", L, "\n", "@deployTime@:"))
  for (i in c(1:dim(tempdat)[1])) {
    cat(paste0('<deploy time="', tempdat[i, 1], '"/>'))
  }
  sink()

  rm(tempdat)
}


#### write out all names
sink(file.path("deployment_times", "All_SweepNames.txt"), append = FALSE)
cat(paste0(as.matrix(gsub(".txt", "", list.files(SweepDir))), "\n"))
sink()


## ==============================================
##### SAVE DATASET
## ==============================================
save(seasonalityDat_long, dataList, dataList_v1, dataList_v2, file = file.path("dat", "TimestepDat.RData"))
