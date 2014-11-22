## The objective here is to calculate the Inverse of a matrix and cache the results
## The next time the inverse of the same matrix is needed, the result is
## returned from the cache and is not re-calculated.


## The makeCacheMatrix function will create a matrix from a given input matrix whose
## inverse can be cached and retrieved later as needed

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## the cacheSolve function will return the inverse of a matrix.
## If the inverse was calculate previously, then the result is returned from
## the cache, otherwise the inverse is calculated and saved into  the cache
## before returning it from the function

cacheSolve <- function(x, ...) {
    if (nrow(x$get()) != ncol(x$get())) {
        message("No possible to invert a rectangular matrix")
        return()
    }
    ## Return a matrix that is the inverse of x
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}


## TEST
## This is a simple test to see if the matrix inversion actually works and
## whether the results are correct.
## Also, it can be seen that the result is returned from the cache
## when the inverse of the same matrix is recalculated

# Create a matrix
my_matrix <- c(1, 2, 3, 4)
dim(my_matrix) <- c(2, 2)
my_matrix

# Calculate its inverse by the usual method to check for correctness
solve(my_matrix)

# Create a cachable matrix
my_cache_matrix <- makeCacheMatrix(my_matrix)
# Calculate its inverse and cache the result
cacheSolve(my_cache_matrix)
# Recaclculate the inverse and verify that the result is returned from the cache
cacheSolve(my_cache_matrix)
