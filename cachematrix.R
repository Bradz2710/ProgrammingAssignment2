# > n <- matrix(rnorm(16), nrow = 4)          // Create a matrix N
# > cn <- makeCacheMatrix(n)                  // Create our special matrix
# > cn$get()                                  // Return the matrix
# > cacheSolve(cn)                            // Return the inverse
# > cacheSolve(cn)                            // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(n = matrix()) {
    inv <- NULL
    set <- function(y) {
        n <<- y
        inv <<- NULL
    }
    get <- function() n
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(n, ...) {
    inv <- n$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- n$get()
    inv <- solve(data, ...)
    n$setinv(inv)
    inv
}
