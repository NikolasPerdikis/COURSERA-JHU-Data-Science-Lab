rankhospital <- function(state, outcome, num = "best") { ## Read outcome data
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
	## create a new dataframe that only contains the required state
	status_message <- c("State = " , state, "Outcome =" , outcome)
## print what we have so far
	print(status_message)
##
if (outcome=="heart attack") {
	outcome_per_state<-outcome_all[which(outcome_all$State==state),c(2,7,11)]

	} 
 else if (outcome=="heart failure") {
 	outcome_per_state<-outcome_all[which(outcome_all$State==state),c(2,7,17)]
 		
		} 
	else if (outcome=="pneumonia") {
		outcome_per_state<-outcome_all[which(outcome_all$State==state),c(2,7,23)]
		
 	}
	}else stop("invalid state")
# Function exits, as there was no correct state given
	} else stop("invalid outcome")
# Function exits, as there was no correct outcome given
	colnames(outcome_per_state)<-c("HospitalName","state","FatalType")
	outcome_per_state[,3]=as.numeric(outcome_per_state[,3])

NA_count <- sum(!is.na(outcome_per_state$FatalType))

if(num=="best"){
    num <- 1
}else if(num=="worst"){
    num <- NA_count
}

sort.outcome_per_state <- outcome_per_state[order(outcome_per_state$FatalType,outcome_per_state$HospitalName,na.last=TRUE),]
print(sort.outcome_per_state[num,1])

options(warn=0)
## Return hospital name in that state with lowest 30-day death 
## rate 11}
}
