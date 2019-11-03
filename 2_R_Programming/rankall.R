
#rankall.R

#R Programming Assignment 3
#Phillip.Escandon@gmail.com
library(dplyr)
options(warn = -1)
rankall <- function(outcome, num = "best"){

       outcomeDF <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
hospitalRankDF <- data.frame(hospital=character(54), state=character(54))

        hosp <- character()

        state <- character()

        

        # use the split - 

        outcomeByState <- split(outcomeDF, outcomeDF$State)

        

        

        stateNames = names(outcomeByState)

        # Create my subset of only the data we are interested in - 

        # should be names here, not numbers

        for(i in 1:length(stateNames)){

                

                mySubset <- select(outcomeByState[[stateNames[i]]],2,7,11,17,23)

                

                # rename to make it readable

                mySubset <- rename(mySubset, Hospital = Hospital.Name)

                mySubset <- rename(mySubset, HeartAttack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)

                mySubset <- rename(mySubset, HeartFailure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)

                mySubset <- rename(mySubset, Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

                

                if(outcome == "heart attack"){

                        mySubset$HeartAttack <- as.numeric(mySubset$HeartAttack)

                        badNA <- is.na(mySubset$HeartAttack)

                        mySubset <- mySubset[!badNA,]

                        mySubset2 <- arrange(mySubset,HeartAttack)

                        mySubset2 <- mySubset2[order(mySubset2$HeartAttack, mySubset2$Hospital), ]

                        if(num == 'best'){

                                hospital <- head(mySubset2$Hospital,1)

                        }else if(num == 'worst'){

                                hospital <- tail(mySubset2$Hospital,1)                        

                        }else{

                                hospital <- mySubset2[num,1]

                        }

                        

                }else if(outcome == "heart failure"){

                        mySubset$HeartFailure <- as.numeric(mySubset$HeartFailure)

                        badNA <- is.na(mySubset$HeartFailure)

                        mySubset <- mySubset[!badNA,]

                        mySubset2 <- arrange(mySubset,HeartFailure)

                        mySubset2 <- mySubset2[order(mySubset2$HeartFailure, mySubset2$Hospital), ]

                        if(num == 'best'){

                                hospital <- head(mySubset2$Hospital,1)

                        }else if(num == 'worst'){

                                hospital <- tail(mySubset2$Hospital,1)                        

                        }else{

                                hospital <- mySubset2[num,1]

                        }

                }else if(outcome == "pneumonia"){

                        mySubset$Pneumonia <- as.numeric(mySubset$Pneumonia)

                        badNA <- is.na(mySubset$Pneumonia)

                        mySubset <- mySubset[!badNA,]

                        mySubset2 <- arrange(mySubset,Pneumonia)

                        mySubset2 <- mySubset2[order(mySubset2$Pneumonia, mySubset2$Hospital), ]

                        if(num == 'best'){

                                hospital <- head(mySubset2$Hospital,1)

                        }else if(num == 'worst'){

                                hospital <- tail(mySubset2$Hospital,1)                        

                        }else{

                                hospital <- mySubset2[num,1]

                        }

                }else{stop("invalid outcome")}

                

                hosp[i] <- hospital

                state[i] <- mySubset2$State[1]

                

                #Put in the dataframe

                #hospitalRankDF$hospital[i] <- hospital

                #hospitalRankDF$state[i] <- state

        }

        hospitalRankDF <- data.frame(hospital = hosp,state = state)

}