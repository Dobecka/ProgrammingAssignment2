## This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()) {
## Contains functions to:
##      setmatrix
##      getmatrix
##      setinverse
##      getinverse

        i = NULL
        set = function(y) {

                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse) i <<- inverse 
        getinv = function() i
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}

## This function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}

}
