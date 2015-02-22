## Allows caching of matrix inversion result, using an object
## (list, defined by makeCacheMatrix) acting as a matrix.  The
## cache is implemented within the object and is flushed (set to 
## null) if object is modified through set().
##
## The following functions are supported by the object
##   Public:
##     set(m = matrix())
##     get(), returns matrix
##   Private, used by cacheSolve:
##     setinverse(m = matrix())
##     getinverse(), returns matrix
##
##  The object also supports list()

## makeCacheMatrix implements the object

makeCacheMatrix <- function(m = matrix()) {
  cache <- NULL
  
  set <- function(m2) {
    m <<- m2
    cache <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setinverse <- function(inverse) { 
    cache <<- inverse
  }
  
  getinverse <- function() { 
    cache
  }
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix defined by 
## makeCacheMatrix.  If the object already has an inverse calculated
## it returns that.  Otherwise, it calculates the inverse.
## 
## cacheSolve(x = makeCacheMatrix(m = matrix())) returns inverted matrix

cacheSolve <- function(m, ...) {
  solution <- m$getinverse()
  
  if(!is.null(solution)) {
    message("using cached data")
  } else {
    matrixdata <- m$get()
    solution <- solve(matrixdata, ...)
    m$setinverse(solution)
  }
  
  solution
}
