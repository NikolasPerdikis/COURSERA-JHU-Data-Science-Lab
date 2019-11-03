#
#
#Nikolaos Perdikis 26.5.2019
# Getting and Cleaning Data Course Project - Peer Graded Assignment
#
#

# DATA:
# 30 age [16,48] volunteers, 
# 6 activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING), 
# 2 sets of data (Training 70%, Test 30%)
#

#
# Install packages necessary
library(dplyr)
library(tidyr)
# 

# Do the file work:
# Establish remote location (specified by the exam) and local file
# Unzip the file and naviate the the root directory of the files
remote_project_data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
local_project_data <- "getdataprojectfilesUCIHARDataset.zip"
if (!file.exists(local_project_data)) {download.file(remote_project_data,local_project_data)}
unzip (local_project_data)
root_working_dir <- "./UCI HAR Dataset"
setwd(root_working_dir)
#

#
# Read the existing files
# create variables equal to the name of each file
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")
# Training Data
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/Y_train.txt")
# Test Data
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/Y_test.txt")
#

#
#Rename some fields to perform the merge(s)
names(activity_labels)[1:2] <- c("activity_code", "activity_name")
names(Y_train)[1] <- c("activity_code")
names(Y_test)[1] <- c("activity_code")
names(subject_train)[1] <- c("test_user_1_of_30")
names(subject_test)[1] <- c("test_user_1_of_30")

#

# Modify the activity so that it contains the activity_name instead of the activity code
# One way is to make a function that will have input the code, and activity the name
# Then we can use mutate function to update the value in our table
return_activity_name <- function(activity_code) {
  activity_labels$activity_name[activity_code]
}
Y_train <- Y_train %>% mutate(activity_name=return_activity_name(Y_train$activity_code)) 
Y_test <- Y_test %>% mutate(activity_name=return_activity_name(Y_test$activity_code))
#

#
#merge the activity type with the measurement and the owner, using cbind
training_data <- cbind(X_train,Y_train,subject_train)
test_data <- cbind(X_test,Y_test,subject_test)
#

#
# Rename the 561 columns, getting their value from features.txt
# To rename the columns in the training and test sets, we need to get the column names from file "features.txt"
# There are 561 rows in the features file, (features$V2 holds the name), and 561 columns in the X_ files
names(training_data)[1:561] <- as.vector(features$V2)
names(test_data)[1:561] <- as.vector(features$V2)
#

#
# Having the training_data dataset in place, we now keep only mean and std columns from columns [1,561]
# we do this by keeping field names with std and mean, plus the owner and the activity name
# we use temporary element res_training and res_test to hold the data
res_training <- training_data[,grep("std|mean",colnames(training_data))]
res_training <- cbind(res_training,training_data$activity_name,training_data$test_user_1_of_30)
res_test <- test_data[,grep("std|mean",colnames(test_data))]
res_test <- cbind(res_test,test_data$activity_name,test_data$test_user_1_of_30)
names(res_training)[80] <- "Activity_Name"
names(res_test)[80] <- "Activity_Name"
names(res_training)[81] <- "User_1_Of_30"
names(res_test)[81] <- "User_1_Of_30"
#

#
# Bind together res_training and res_test, with proper column names and only mean/std values into one element
unified_data <- rbind(res_training,res_test)
names(unified_data)[81] <- "Participating_User_No"
# and confirm that number of columns has been preserved, also Rows_Test + Rows_Training = Rows_Unified
dim(res_training)
dim(res_test)
dim(unified_data)
#

#
# Now to transform the table: We use tidyr::gather to keep the Activity Name and the user, but transform all the rest into 
# 2 columns: One that has the Measurement Name, and the other that has the measurement value, (plus, user name and activity type)
unified_data <- unified_data %>% tidyr::gather(Measurement_Name,Measurement_Value,-Activity_Name,-Participating_User_No)
#


#
# Last, we employ group_by and summarize to extract the data for our new tidy data set, and then save it to a file
unified_data_mean_per_activity_user <- 
  unified_data %>%
  dplyr::group_by(Participating_User_No,Activity_Name,Measurement_Name) %>%
  dplyr::summarize(Mean_Measurement_Value=mean(Measurement_Value))
#

# save to file
write.table(unified_data_mean_per_activity_user,"tidy_Topic_5.txt",row.name=FALSE)
#
