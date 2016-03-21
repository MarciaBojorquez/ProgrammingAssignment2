## Marcia Vianey Bojorquez Avitia
## Coursera: Module 2, week 3


## Get the inverse of a matrix, if it has been already calculated the 
## already computed value

## Make a special matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solve the inverse of a matrix, if it has been already calculated
## the the inverse from the cache. Otherwise, calculate the inverse

## x our special matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
