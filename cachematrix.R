## makeCacheMatrix and cacheSolve work in tandem to compute and
## cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Returns the following list of functions:
        ##
        ## set(y)     takes a matrix y and sets it equal to the variable x
        ## get()      returns the value of x
        ## setinv(y)  sets a matrix y to the variable inv
        ## getinv()   returns the value of inv

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse_matrix) inv <<- inverse_matrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

        ## Usage:   > x <- makeCacheMatrix(<matrix>)
        ##          > cacheSolve(x)

        ## If the inverse of 'x' has already been
        ## calculated, then return inv.
        ## Otherwise, compute the inverse, store
        ## the inverse in inv, and return inv.

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        A <- x$get()
        inv <- solve(A)
        x$setinv(inv)
        inv
}
