# good-day
## Functions that cache the inverse of a matrix
 #' Util function that set the matrix and the inverse in an environment
 #' @param x an invertible matrix
> #' examples
> #' x = makeCacheMatrix(matrix(rnorm(25), 5, 5))
> #' x$set(matrix(rnorm(4), 2, 2))
> makeCacheMatrix <- function(x = matrix())
 {
   # todo error if x is not a matrix
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
 
 #' Solve and cache the inverse of a matrix
 #' @param x the result of a previous makeCacheMatrix call
 #' @param ... additional arguments to pass to solve function
 #' example
 #' x = makeCacheMatrix(matrix(rnorm(25), 5, 5))
 #' cacheSolve(x)
 cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cached matrix inverse")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv }
