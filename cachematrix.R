## Matrix inversion is a costly operation, and caching the result 
## will be beneficial if we need to compute the inverse repeatedly.
## Following two functions "makeCacheMatrix" and "cashSolve" 
## are used to compute and cache the inverse of a matrix.

## makeCacheMatrix function will take matrix as an input parameter 
## and returns a special vector with list of following functions
##   1. Set - to set the value of matrix, 
##      and resets computed inverse of matrix value if any
##   2. get - to get the value of matrix
##   3. setinverse - to set the value of inverse of the matrix
##   4. getinverse - to get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## cachSolve function takes special vector created from 
## makeCacheMatrix function as input and returns inverse of matrix. 
## While returning, it checks if inverse of matrix is already computed 
## and cached using getinverse function
##   if so it return the cached value
##   if not it computes the inverse, and sets the value in the cache 
##     using makeCacheMatrix setinverse function
## This function assumes matrix is always invertible
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}

## Sample test run
## > x<-matrix(1:4,2,2)
## > m<-makeCacheMatrix(x)

## First run without any cache
## > cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Second run with cache value
## cacheSolve(m)
## getting cached data.
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5