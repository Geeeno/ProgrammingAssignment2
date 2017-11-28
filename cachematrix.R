## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(invermat) invm <<- invermat
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
    
}
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    mat0 <- x$get()
    invm <- solve(mat0, ...)
    x$setinvm(invm)
    invm    
}