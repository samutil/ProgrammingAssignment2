#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  invMatrix <- NULL
  
  #set the Matrix
  
  setMatrix <- function(y) {
    x <<- y
    
    invMatrix <<- NULL
    
  }
  
  #get the Matrix
  getMatrix <- function()
    x 
  #set the invertible matrix
  setInverse <-function(inverse)
      invMatrix <<- inverse 
  
  #get the invertible matrix
  getInverse <-function()
      invMatrix 
  
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    
    setInverse = setInverse,
    getInverse = getInverse
  )
  

}




#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #get the value of the invertible matrix from the makeCacheMatrix function
  
  invMatrix <- x$getInverse()
  
  #if inverse matrix is not NULL, return a message
  
  if (!is.null(invMatrix)) {
    
    message("Getting Cached Invertible Matrix") 
    return(invMatrix) 
    
  }
  
  
  
  #if the invertible matrix is NULL 
  
  MatrixData <- x$getMatrix() #get the original Matrix 
  
  invMatrix <- solve(MatrixData, ...) #inverse the matrix
  
  x$setInverse(invMatrix) #set the invertible matrix
  
  return(invMatrix) #return the invertible matrix
  
  ## Return a matrix that is the inverse of 'x'
  
}
