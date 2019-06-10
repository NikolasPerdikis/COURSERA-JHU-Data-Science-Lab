## Put comments here that give an overall description of what your
## functions do

## NIKOLAS PERDIKIS Week3 Exercise
## Before the actual code, and based on the instructions at
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
## the first thing i would like is to present my test case
##
## So the test case is creating a 2x2 matrix, using a very simple function:
## mNP <- matrix(rnorm(4),nrow=2,ncol=2)
##> mNP
##          [,1]       [,2]
##[1,] 0.5644179 -0.4694577
##[2,] 0.7707571  0.1500823
##
## ...with the hope that it is invertible...for this, we define the inverse matrix, 
## using function solve(): 
## > mNP_inv <- solve(mNP)
## > mNP_inv
##           [,1]     [,2]
## [1,]  0.3360953 1.051306
## [2,] -1.7260381 1.263961
## and finally, the confirmation
##> mNP %*% mNP_inv
##     [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 
makeCacheMatrix <- function(x = matrix()) {
##
## RUN AS: <new_matrix_name> = makeCacheMatrix(<matrix_we_know_has_inverse>)
##
## This function has been very closely modelled by the example given by the professors
## following the description of the exercise, just some functions had to be swapped as the 
## example deals with vectors and the function is mean(), whereas the exercise at hand
## required matrices and their inversion.
## $set function: what this does is use the '<<-' operator to pass the matrix given in the function 
## to the global environment. Since this means by definition (R stores all env in-memory) that this
## object is now cached
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
 ## $set function does nothing more than display the cached object. it is identical as displaying the
 ## original object (the one we passed as argument to this function)
  get <- function() x
  ## setinverse acts equally as above, but for the inverse matrix (using function solve()), which is also
  ## passed to the global environment
  setinverse <- function(solve) m <<- solve
  ## works similarly as $get, just using the variable "m" which has now stored the result of the inversion
  getinverse <- function() m
## Finally, create the list. now each item is accessible based on the object to which the result of our 
## function was assigned. for example if the original matrix we had defined was M1, we could run this
## function as x <- makeCacheMatrix(M1). In this case, we can use the elements of the list as
## x$set,x$get,x$setinverse,x$getinverse 
## (see below the cacheSolve function and it all becomes clear ;) )
  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
	##RUN AS: cacheSolve(<new_matrix_name>) , please see line #31 of this file for explanation
    ## Now that we have the list of the 4 functions (set, get, setinverse,getinverse) in the global environment
    ## we can creatively run them
    m <- x$getinverse() 
    ## we are calling the getinverse function which we defined with makeCacheMatrix
    if(!is.null(m)) {
    ## m will always be NULL the first time we run cacheSolve, as the list in cacheMatrix has taken care of that
    ## see lines #40 and #43 of this file for details
    ## of course if the list is large enough, exceeding the memory resource of the computer running this script,
    ## then this may never be the case, and the inversion has to happen always on the fly
      message("getting cached data")
      return(m)
      }
    data <- x$get() 
    ## Line above fetches the matrix into this local variable
    m <- solve(data,...) 
    ## ...and performs the inversion
    ## the 3 dots are not necessary, as we run the function with only one argument. However they can be used,
    ## since there is a provision for extra arguments in the definition of the function
    x$setinverse(m)
    ## This line will update the list, so that the next time we run the function, m will be instantiated, and therefore not null
    ## so clause in line #73 will return TRUE and the "getting cached data" message will be displayed
    m
## notice how variable m is not passed back to the global env (just type its name in the R prompt)
## unlike the list created by the other function, for which the operator <<- was used.
## if you want to have the function recompute the inversion, perform rm(<new_matrix_name>)
}
## 
## MANY THANK YOU FOR REVIEWING, HAVE ALL A GREAT DAY!!
## NIKOLAS PERDIKIS 7/11/2018
