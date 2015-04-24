## Create a special matrix object that can cache its inverse i. 'x' is a square
## invertible matrix.

## First function creates a list containing a function to:
## Set the matrix
## Get the matrix
## Set the inverse
## Get the inverse 

makeCacheMatrix <- function(x = matrix() ) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
            }
    get <-  function() x
            setinv <- function(inv) i <<- inv
            getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Next compute the inverse of the special matrix 'x' returned by
## makeCacheMatrix. Use the above list as input to the function cacheSolve(). 

## First, check if inverse already calculated. If so, get it from the cache and 
## skip the computation. 
## Otherwise, calculate the inverse of the matrix and set its value in the cache
## via the setinv function.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i) ) {
            message("getting chached data")
            return(i)
            }
    data <- x$get()
            i <- solve(data, ...) 
            x$setinv(i)
            i
}
