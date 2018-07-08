pollutantmean <- function(directory, pollutant, id = 1:332) { #as instructed
        csv__dir_list <- list.files(directory, full.names=TRUE, pattern="*.csv") #need to catch only CSV files contained in the folder
        dfCSV = data.frame() #initialize an empty data frame. Columns will come with the first rbind
        for (i in id) { #loop through all the files in the vector
            
                dfCSV <- rbind(dfCSV, read.csv(csv__dir_list[i]))    #Append each CSV within the vector to the data frame
        }
        dfCSV_vectored <- dfCSV[which(dfCSV[,"ID"] %in% id),] 
        mean(dfCSV_vectored[, pollutant], na.rm=TRUE) #Calculate the mean, mandatory to exclude NA values
}
