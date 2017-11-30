## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This file contains a pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      list( set=set,
            get=get,
            setInv = setInv,
            getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      dataMatrix <- x$get()
      inv <- solve(dataMatrix, ...)
      x$setInv(inv)
      inv
}

## Test function
# A <- matrix(c(2,2,3,2),2,2)
# xa <- makeCacheMatrix(A)
# cacheSolve(xa)
# 
# B <- matrix(c(1,-1,1,2),2,2)
# xb <- makeCacheMatrix(B)
# cacheSolve(xa)







