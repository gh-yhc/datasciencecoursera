
# Put comments here that give an overall description of what your
# functions do
# The following two functions cache the inverse of a matrix

# Write a short comment describing this function
# This makeCacheMatrix function takes a matrix input x and 
# outputs a list containing functions to set/get the 
# value/inverse of x.
makeCacheMatrix <- function(x = matrix()) {
  minv <- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))
  set <- function(y) {
    x <<- y
    minv <<- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))
  }
  get <- function() x
  setinv <- function(solve) minv <<- solve
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Write a short comment describing this function
# The following function cacheInv calculates the inverse of the matrix 
# created with the above function. However, it first checks to see if the inverse 
# has already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!all(is.na(minv))) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}

# Example
x<-matrix(data=c(2.3,0.5,-0.7,-0.34),nrow=2,ncol=2)
a<-makeCacheMatrix(x)
cacheSolve(a)
