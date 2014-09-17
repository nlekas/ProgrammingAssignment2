## makeCacheMatrix makes a speical matrix that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## makeCacheMatrix sets up the special matrix for inversion
## call to it by using the built in commands: get, setinv, getinv, set
## example: z$get()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        ## Returns the Matrix
        get <- function() x
        ## Caches inverse
        setinv <- function(solve) m <<- solve
        ## Returne inverse of matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve checks if there is an inverted matrix cached.
## If not, it recomputes it
## If so, it returns the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## checks if inverse is cached
    ## If so, then returns cached inverse
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If inverse is not cached then
    ## Computes inverse, caches it, and returns it
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
