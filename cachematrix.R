## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special "matrix" object that can cache its inverse by making a list that contains a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.
#Otherwise, it will create the inverse matrix and sets it in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

