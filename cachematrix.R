## These functions should create a matrix, test to see if matrix exists, 
## if so, returns cache, if not, calculates inverse

## makeCacheMatrix 
## set value of matrix
## get value of matrix
## set value of inverse
## get value of inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set value for matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get value for matrix
        setinverse <- function(solve) m <<- solve ## sets calculated inverse to cache
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will perform the inverse on the object if it is not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## set data to defined matrix
        m <- solve(data) ## set m to inverse of data
        x$setinverse(m) ## sets inverse in cache
        m ## print inverse solution
}
