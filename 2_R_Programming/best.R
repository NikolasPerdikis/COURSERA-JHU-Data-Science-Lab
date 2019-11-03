best <- function(state, outcome) { 
## Read outcome data
options(warn=-1)
outcome_all <- read.csv("outcome-of-care-measures.csv",colClasses="character")
## Get the state, validate it, then create a subset just for the state
## the validity for the state argument is that it must be a value in the column 7
## the validity for the outcome is that it must be one of the values "heart attack/heart failure/pneumonia"
outcome_valid_values <- c("heart attack","heart failure","pneumonia")
##The columns needed for this exercise are

if (outcome %in% outcome_valid_values == TRUE) {
## check if outcome value is within the vector of correct values
	if (state %in% outcome_all$State) {
## check if there was a valid state argument given
## for this we try to find the passed argument in the State column of the dataframe
	outcome_per_state<-outcome_all[which(outcome_all$State==state),]
## create a new dataframe that only contains the required state
	status_message <- c("State = " , state, "Outcome =" , outcome)
## print what we have so far
	print(status_message)
##
if (outcome=="heart attack") {
		outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		min_heart_attack_val <- min(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=TRUE) 
		row_index_in_df=which(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min_heart_attack_val)
		print(outcome_per_state$Hospital.Name[row_index_in_df])
	} 
 else if (outcome=="heart failure") {
 		outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		min_heart_failure_val <- min(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm=TRUE) 
		row_index_in_df=which(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min_heart_failure_val)
		print(outcome_per_state$Hospital.Name[row_index_in_df])
		} 
	else if (outcome=="pneumonia") {
		outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		min_pneumonia_val <- min(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm=TRUE) 
		row_index_in_df=which(outcome_per_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_pneumonia_val)
		print(outcome_per_state$Hospital.Name[row_index_in_df])
 	}
	}else stop("invalid state")
# Function exits, as there was no correct state given
	} else stop("invalid outcome")
# Function exits, as there was no correct outcome given
rm(outcome_all)
options(warn=0)
## Return hospital name in that state with lowest 30-day death 
## rate 11}
}
