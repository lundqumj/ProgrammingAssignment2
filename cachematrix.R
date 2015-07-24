## The purpose of this script is to compute the inverse of a matrix and
## cache the result for later use.  The script contains two functions:
## one function to load the set/get functions
## one function to comput the inverse and save the result to cache.
## SCROLL DOWN FOR TESTING SCENARIO


## this first function defines and loads the functions needed to cache a matrix and it's inverse
## the function returns a vector(list) containing the functions to
##   1 set the value of the matrix
##   2 get the value of the matrix
##   3 set the inverse of the matrix
##   4 get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL						#this variable will store the inverse matrix
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## this second function receives a matrix 'x' and returns the inverse of the matrix
## using the cached inverse if available

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



############################################################################
##  steps for testing the script
##
##  1. place this script in your working directory
##       or setwd(<directory path>) to where this script is stored
##		> setwd(paste(getwd(),"/ProgrammingAssignment2",sep=""))
##  
##  2. import the script
##  		> source('cachematrix.R')
##  
##  3. define a matrix.  For example:  a 2 X 2 matrix with values 1, 2, 3, 4
##  		> my_matrix <- matrix(1:4,nrow = 2, ncol = 2)
##  		> my_matrix
##		     [,1] [,2]
##		[1,]    1    3
##		[2,]    2    4
##
##  4. execute the first function.  save the result into variable 'm'
##  		> m <- makeCacheMatrix(my_matrix)
##  		> m
##  		   # you should see the four set/get functions in the environment
##		
##		> m$get()		# verify matrix has been loaded to memory
##		     [,1] [,2]
##		[1,]    1    3
##		[2,]    2    4
##
##		> m$getinverse()
##		NULL			# inverse has not been loaded to cache
##  
##  5. execute the Solve function.  
##		> cacheSolve(m)
##	     		[,1] [,2]
##		[1,]   -2  1.5
##		[2,]    1 -0.5
##
##		> m$getinverse()
##		     [,1] [,2]
##		[1,]   -2  1.5
##		[2,]    1 -0.5	# inverse has been loaded to cache
##
##  6.  run the Solve function again.  This time m is not null and it should read from cache
##		> cacheSolve(m)
##		getting cached data	# message displays this time
##		     [,1] [,2]
##		[1,]   -2  1.5
##		[2,]    1 -0.5
##		
############################################################################



