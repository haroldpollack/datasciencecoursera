## Put comments here that give an overall description of what your
## functions do
##############
#    this program inverts X and saves it in the cache
#
#
#############
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
#
#       This one returns a matrix that is x-inverse
#
################################
cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
