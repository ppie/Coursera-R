##Caching the inverse of a matrix
#comments are more less the same as in example from assignment

## makeCacheMatrix - creates a list containing a function to:
## set - set the value of the matrix
## get - get the value of the matrix
## set.inverse - set the value of the inversed matrix
## get.inverse - get the value of the inversed matrix
 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  set.inverse <- function(solve) m <<- solve
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  
}


## cacheSolve - calculates the inverse of the matrix
## It first looks for the result in get.inverse method
## If the inverse has already been calculated it just gives a result
## Otherwise, it calculates the inverse and sets that value in cache


## It assumes that matrix x is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m  
}


##Some tests
##
##n = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##n$get() 
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
##cacheSolve(n)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##Same result as using cacheSolve
##n$get.inverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##Retrieving from cache, second run
##cacheSolve(n) 
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##n$set(matrix(c(-4,5,10,0.5), nrow=2, ncol=2))
##First run of new matrix, it must by calculated
##cacheSolve(n)
## [,1]       [,2]
## [1,] -0.009615385 0.19230769
## [2,]  0.096153846 0.07692308
##
##n$get()
## [,1] [,2]
## [1,]   -4 10.0
## [2,]    5  0.5

