##Final Answer on 04/24
## Caching the Inverse of a Matrix, get and set values of it by below 2 functions

## Creates a special "matrix" object that can cache its inverse, which is really a list containing a function to
# 1.set the value of the Matrix
# 2.get the value of the Matrix
# 3.set the value of the Matrix Inverse
# 4.get the value of the Matrix Inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(mar) {
    x <<- mar
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get,setInverse = setInverse, getInverse=getInverse)

}

#  Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

