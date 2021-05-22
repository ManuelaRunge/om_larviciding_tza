#<MRunge, 05.04.2018>
### function to convert a date into a 5 day timestep and vice versa for OpenMalaria


##===========DATE TO TIMESTEP FUNCTION ============ 
  dateToTimestep <- function(datevar, SIMSTART , digits = 1, miss = NULL, label = FALSE, print = TRUE) {
  out <- as.numeric(round((datevar-SIMSTART)/5+1,0))
  return(out)
}
TimestepToDate <- function(timesteps, SIMSTART , digits = 1, miss = NULL, label = FALSE, print = TRUE){
	out <-as.Date(timesteps*5, origin=SIMSTART)
	return(out)
}

### Define defaults simulation start date
##===========SIMULATION START=================== 
dateStart	<- as.Date("1957-01-01")
##==============================================
print(paste0("Default: simulation start in ", as.character(dateStart)))
print(paste0("object saved in 'dateStart'"))

### Example of how to use function and to export dates to be included in xml
##===========EXAMPLE & EXPORRT =================== 
EXAMPLE=FALSE
if(EXAMPLE){
	
	# one date
		datevar <- as.Date("2016-06-15")
		(timesteps <- dateToTimestep(datevar,dateStart))
	
	# multiple dates
		datevar <- seq(as.Date("2016-01-01"), as.Date("2026-01-01"), "years")
		(timesteps <- dateToTimestep(datevar,dateStart))
	
	# write lines for xml
	timedatematrix <- cbind(as.data.frame(timesteps), as.data.frame(datevar))
	
	for(i in 1:dim(timedatematrix)[1]){
		cat(paste0("<surveyTime>",timedatematrix[i,1],"</surveyTime>", " <!--", as.character(timedatematrix[i,2]),"-->", "\n"))
		}
	
	#--->  from here copy and paste into xml, or export as text file
	# Export 
	sink(paste0("surveyTimeStepsXML.txt"), append=TRUE)
	for(i in 1:dim(timedatematrix)[1]){
		cat(paste0("<surveyTime>",timedatematrix[i,1],"</surveyTime>", " <!--", as.character(timedatematrix[i,2]),"-->", "\n"))
	}
	sink()
}
##==============================================