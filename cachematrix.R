## Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## The function makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. The function contains: 

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix


makeCacheMatrix<-function(x=matrix()) {
  i <- NULL
## Set matrix  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
## Get matrix
  get <- function() x
## Set and get the inverse of the matrix  
  setinverse <- function(inverse) 
    i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
## Compute the inverse of 'x'  
  i <- x$getinverse()
## If the inverse has already been calculated - return it  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) 
  x$setinverse(i)
  i
}
