## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.


## Write a short comment describing this function

## # makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {

## use `<<-` to assign a value to an object in an environment 
## different from the current environment.     
    
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()  

  
  inv <- x$getinverse()
  
  
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test makeCacheMatrix(x) & cacheSolve(m):

x = rbind(c(1, 9), c(9, 1))
m = makeCacheMatrix(x)
m$get()

##       [,1] [,2]
##[1,]    1    9
##[2,]    9    1

##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,]  -0.25  1.00

cacheSolve(m)


##           [,1]   [,2]
## [1,]  -0.0125  0.1125
## [2,]   0.1125 -0.0125
