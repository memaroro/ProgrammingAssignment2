##THIS FUNCTION CREATES A SPECIAL MATRIX (makeCacheMatrix)
##and makes a list of 4 variables, which you will use in the next function.
##
##The Set function is setting the cached numeric vector and is setting the cached "m" to NULL
##The get function is returning the cached numeric vector
##The setsolve requires a "special" matrix that you can compute its inverse(solve function)
##              and will set the cache matrix
##The getsolve returns the inverse of the cache matrix

makeCacheMatrix <- function(x = numeric()){
  m <- NULL

  set <- function(y){
        x <<- y
        m <<- NULL
  }
  
  get <- function()x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
} 

## And with the cacheSolve function you will have the inverse of the cache matrix.
## Calculates the inverse of the matrix(with the solve function) obtained from the makeCacheMatrix object.
##First you will get the cache inverse of the matrix with "x$getsolve()" and assign it locally into "m"
##if "m" is not NULL will return the cache inverse obtained from the makeCaceMatrix function.
##if "m" is NULL then you get the cache matrix obtained from the makeCaceMatrix function 
##   and assign it to the data variable
##Finally calculates the inverse of the data variable and returns this calculated inverse

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
