## The two functions allow to creates a special "matrix" object that can cache it's inverse (using makeCacheMatrix) 
## and retrieve it, either by computation or cache reading (using cacheSolve).

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## The cacheSolve function take a special "matrix" returned by the makeCacheMatrix function as input
## and return the inverse of the matrix. The inverse is either retrieved from cache if already computed,
## and the matrix being unchanged, else it's calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}