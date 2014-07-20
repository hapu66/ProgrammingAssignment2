##    The functions practice lexical scoping and cacheing for the purpose of matrix inverse. 
##

## makeCacheMatrix creates a matrix,  and retuns it and its inverse
##                 with methods  get() and getmrx().   the inverse can be re-set with method set(),
##                  but usually this is not advisable.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmrx <- function(solve) m <<- solve
	  getmrx <- function() m
        list(set = set, get = get,
             setmrx = setmrx,
             getmrx = getmrx)
}
# cacheSolve returns the inverse of matrix x,  from  cache if existing, otherwise calculates it

cacheSolve <- function(x, ...) {
        m <- x$getmrx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mrx <- x$get()
        m <- solve(mrx, ...)
        x$setmrx(m)
        m
}

