# Getting_And_Cleaning_Data

## The project for Course #3 of Johns Hopkins University Data Science Specialization 
 
The script contained is called run_analysis.R
The script will download the remote data in a local file, then unzip the local file. Since the zip will 
create directory "UCI HAR Dataset", the script will setwd to this path. 
Then all relevant files are being read, *to dataframes that are named after the file that contains their data*

Key Functions used for the preparation of the data:
* A user defined function called return_activity_name, which converts the activity code to the corresponding name
* dplyr::mutate to add columns
* cbind to attach the user and the activity name to the measurements
* rbind to merge the training and the test data
* tidyr::gather to "invert" the collection, so create two new columns and add as row data the previous column names and their values 
(excluding Participant Name and Activity Name)
* dplyr::group_by and summarize to group all measurement for the same Participant Name and Activity, in order to acquire their mean
* write.table to extract unified_data_per_activity_user to a file

The final output is a data frame called *unified_data*. It has column *Participating_User_No* and *Activity_Name*, along with n columns, 
named for the measurement they represent and having the value of the measurement

The second output, which is attached in the file uploaded to the project, is a "grouped_df" object called *unified_data_per_activity_user*
This object contains for 4 columns, again *Participating_User_No* and *Activity_Name*, and 2 more columns which contain in one the name of the measurement, and to the other its value
 
