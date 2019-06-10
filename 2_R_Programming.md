Week 2 -> Programming Assignment 1 

Part 1

Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

Link to file: https://github.com/NotNikolas63500/COURSERA-JHU-Data-Science/blob/master/pollutantmean.R

Part 2

Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

Link to file: 

Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

Link to file:

Week 3 -> Programming Assignment 2

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

https://github.com/NotNikolas63500/COURSERA-JHU-Data-Science/blob/master/cachematrix.R

Week 4 <- Programming Assignment 3 (Hospital Quality)

Finding the best hospital in a state
Write a function calledbestthat take two arguments:  the 2-character abbreviated name of a state and anoutcome name.  The function reads theoutcome-of-care-measures.csvfile and returns a character vectorwith  the  name  of  the  hospital  that  has  the  best  (i.e.   lowest)  30-day  mortality  for  the  specified  outcomein that state.  The hospital name is the name provided in theHospital.Namevariable.  The outcomes canbe one of “heart attack”, “heart failure”, or “pneumonia”.  Hospitals that do not have data on a particularoutcome should be excluded from the set of hospitals when deciding the rankings.

https://github.com/NotNikolas63500/COURSERA-JHU-Data-Science/blob/master/best.R

Write a function called rankhospital that takes three arguments:  the 2-character abbreviated name of astate (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).The function reads theoutcome-of-care-measures.csvfile and returns a character vector with the nameof the hospital that has the ranking specified by thenumargument

https://github.com/NotNikolas63500/COURSERA-JHU-Data-Science/blob/master/rankhospital.R

Write a function calledrankallthat takes two arguments: an outcome name (outcome) and a hospital rank-ing (num).  The function reads theoutcome-of-care-measures.csvfile and returns a 2-column data framecontaining the hospital in each state that has the ranking specified innum.  

https://github.com/NotNikolas63500/COURSERA-JHU-Data-Science/blob/master/rankall.R