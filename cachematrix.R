## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse ) v <<- inverse 
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## first check if the inverse has been calculated and cached
cacheSolve <- function(x, ...) {
  #I <-matrix(0,nrow=nrow(x),ncol=ncol(x))
  #I[row(I) == col(I)] <-1
  #solve(x,I)
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
  
  
}
