##THIS FUNCTION CREATES A SPECIAL MATRIX (makeCacheMatrix) that can cache its inverse
##This function creates 4 different functions that will be used in the CacheSolve function:
##
##1. getmatrix() -- get the value of the existing matrix
##2. setmatrix() -- set the value of the existing matrix
##3. setinversematrix() -- calculate the inverse matrix 
##4. getinversematrix() -- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
  Inverse <- NULL
  
  ## Setmatrix will null the inverse and will set the input matrix to x
  setmatrix <- function(y){
        Inputmatrix <<- y
        Inverse <<- NULL
  }
 
 ## getmatrix will return the input matrix  
  getmatrix <- function() Inputmatrix
  
  ## setinversematrix will calulate the inverse
  setinversematrix <- function(Input) Inverse <<- solve(Input)
  
  ## getinversematrix will produce the inverse matrix as output
  getinversematrix <- function() Inverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversematrix = setinversematrix, getinversematrix = getinversematrix)
  
} 

## cacheSolve ------------------------------------------------
## And with the cacheSolve function you will have the inverse of the cache matrix.
## Calculates the inverse of the matrix(with the solve function) obtained from the makeCacheMatrix object.
##First you will get the cache inverse of the matrix with "x$inversematrix()" and assign it locally into "output"
##if "outpt" is not NULL will return the cache inverse obtained from the makeCaceMatrix function.
##if "output" is NULL then you get the cache matrix obtained from the makeCaceMatrix function 
##   and assign it to the data variable
##Finally calculates the inverse of the data variable and returns this calculated inverse

cacheSolve <- function(x, ...){
         output <- x$getinversematrix()
         if (!is.null(output)){
          message("getting cached data")
           return(output)
         }
         data <- x$getmatrix()
         output <- setinversematrix(x)
         x$setinversematrix(output)
         output
}
