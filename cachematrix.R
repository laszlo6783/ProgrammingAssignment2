##  We design a pair of functions that cache the inverse of a matrix.
##  In case that the inverse is calculated, we use a property of the R objects.
##  This mean that we use the getinv function to get the inverse.

## makeCacheMatrix creates a special "matrix" (object) which is really a list containing a functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        

}


## The next function calculates the inverse of the matrix
## It first checks if the inverse is calculated. If gets, stops computation and gets the inverse
## Otherwise it calculates the inverse of the matrix and store in the object via setiverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
        
}
