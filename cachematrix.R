## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse of a matrix
  setinverse <- function(inverse) m <<- inverse
  # get the value of the inverse of a matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # calculate the inverse of a matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# run the function
# x<-matrix(c(1, 3, 5, 7), 2, 2)
# ma <- makeCacheMatrix(x)
# cacheSolve(ma)