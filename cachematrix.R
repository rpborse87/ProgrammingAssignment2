getwd()
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Return the matrix from Cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Cashed data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
