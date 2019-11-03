## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes in a matrix as an input 
## It sets and gets the value of the matrix, as well as the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMat <- function() x
  setInv <- function(inverse)  invMat <<- inverse
  getInv <- function() invMat
  list (setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function
## This function checks if the inverse matrix has been calculated
## If so, it takes the inverse matrix from the cache. Otherwise, it will compute
## the inverse of the matrix using the solve function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invMat <- x$getInv()
  if(!is.null(invMat)) {
    message("Getting cached invertible matrix")
    return(invMat)
  }
  MatData <- x$getMat()
  invMat <- solve(MatData, ...)
  x$setInv(invMat)
  invMat
}
