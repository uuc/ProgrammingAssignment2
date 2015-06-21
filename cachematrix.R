## This function creates a special "matrix" object that can cache its inverse.

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL # initialize to NULL
    # create the matrix in the working environment
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x  # get the value of the matrix
    setmatrix <- function(inverse) cache <<- inverse  # invert the matrix and store in cache
    getinverse <- function() cache  # get the inverted matrix from cache
    list(set = set, get = get, setmatrix = setmatrix, getinverse = getinverse)
    ## return the list to the working environment
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache <- x$getinverse() ## get the inverse of the matrix stored in cache
    
    ## return inverted matrix from cache if it exists, 
    ## else create the matrix in working environment
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    matrix <- x$get()
    cache <- solve(matrix, ...)
    x$setmatrix(cache)
    cache
}
