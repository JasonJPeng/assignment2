#    
#   create a matrix newMatrix and myMatrix <- makeCacheMatrix(newMatrix)
#   myMatrix will cache newMatrix if it has been calculated
#   to retrieve the inverse (or solve()) matrix, just call 
#     cacheSolve(myMatrix) it will calculate the inverse 
#      or retreve from teh cache    
#

#  makeCacheMatrix is just a place holder, it doen't do teh calculation
#
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}