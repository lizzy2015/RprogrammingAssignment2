## These functions are caching the inverse of a Matrix.
## We assume that the matrix supplied is always invertible.


rm(list=ls())

## This function creates a special "Matrix" object that can cache its inverse.
## This function is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m.inverse = NULL
  set <- function (y) {
    x<<-y
    m.inverse <<- NULL
  }
  
  get <- function() x
  setinverse=function(inverse) m.inverse <<- inverse
  getinverse=function() m.inverse
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been cacluated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #check if the inverse already exists. If exists, retrieve it.
  m.inverse=x$getinverse()
  if (!is.null(m.inverse)) {
    message("getting cached data")
    return(m.inverse)
  }
  
  #if the inverse doesn't exist, calcute it by using solve()
  data=x$get()
  m.inverse=solve(data)
  x$setinverse(m.inverse)
  
  ## Return a matrix that is the inverse of 'x'
  m.inverse
  
  
        
}

##Example to apply these function
a=matrix(c(1,2,3,4), 2,2)
b=cacheSolve(makeCacheMatrix(a))
b
a%*%b

