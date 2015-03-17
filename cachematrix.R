##This function cache the matrix that you had passed it.
##The function makes a list of 4 variables, which you will use in the next function.

makeCacheMatrix <- function(x = numeric()){
  m <- NULL

  set <- function(y){
        x <<- y
        m <- NULL
  }
  
  get <- function()x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
} 


## This function computes de inverse of the cache matrix. The solve function is for calculate the inverse of
## a square matrix.

cacheSolve <- function(x, ...){
         m <- x$getsolve()
         if (!is.null(m)){
           message("getting cached data")
           return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setsolve(m)
         m
}
