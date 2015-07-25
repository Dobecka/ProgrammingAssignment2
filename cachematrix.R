## This function creates a "matrix" object that can cache its inverse.
## Contains functions to:
##      setmatrix
##      getmatrix
##      set inverse
##      get inverse

makeCacheMatrix <- function (x = matrix()) {

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


##
## 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
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
