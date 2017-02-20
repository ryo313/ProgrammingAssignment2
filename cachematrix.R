## the funtion below contains two elements. makeCachematrix is responsible for
## creating a matrix that has four components: set, get, setsolve, getsolve
## cacheSolve utilizes the four components of makeCacheMatrix and returns a new
## inverse from scratch, or returns a cached inverse. 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # m is set as a placeholder for future value
        set <- function(y) {
                x <<- y
                m <<- NULL # reset the inverse, m,  to NULL
        }
        get <- function() x # returns an original matrix
        setsolve <- function(solve) m <<- solve  
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } # if m = null, that means m was already cached. 
        data <- x$get() # otherwise load the matrix x
        m <- solve(data, ...) # and calculate its inverse from scratch
        x$setsolve(m) # setsolve was updated
        m
}
