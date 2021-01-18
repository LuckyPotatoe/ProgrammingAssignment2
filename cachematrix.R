## Put comments here that give an overall description of what your
## functions do

## Works pretty much the same way as makeVector. 
## The difference is that makeCacheMatrix uses matrix as an input and perform solve operation instead of mean 

#X variable is used to store original matrix values
makeCacheMatrix <- function(x = matrix()) {
  # Variable used to store inv values
  m <- NULL
  
  # Setter, and getter function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #set the value of m to be the inverse of a matrix
  setinv <- function(solve) m <<- solve
  
  #getting the value of the inverted matrix
  getinv <- function() m
  
  #return value of makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Again, works the same as cacheMean, since there's no need for further modification

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  #null check(if there's cached data, data is fetched)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #getting the value of the original matrix
  data <- x$get()
  
  #inverse operation
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
