

# - makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
# - cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#                 If the inverse has already been calculated (and the matrix has not changed), 
#                 then the cachesolve should retrieve the inverse from the cache.

## About makeCacheMatrix()

# makeCacheMatrix() creates a special "matrix", which is really a list containing a function to
# 
# - set the value of the matrix:- set()
# - get the value of the matrix:- get()
# - set the value of the inverse of the matrix:- setinverse()
# - get the value of the inverse of the matrix:- getinverse()

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ix <<- inverse
    getinverse <- function() ix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## About cacheSolve()

# The following function calculates the inverse of the special "matrix" created with the above function. 
# - It first checks to see if the inverse has already been calculated. 
# - If so, it gets the inverse from the cache and skips the computation. 
# - Otherwise, it calculates the inverse of the given matrix
# - And sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setinverse(ix)
    ix
}

# testing ####

myMat <- matrix(1:4, nrow = 2)      # create a normal matrix
myMatVec <- makeCacheMatrix(myMat)  # create a special matrix using the above matrix
myMatVec$get()                      # retrieve the matrix
myMatVec$getinverse()               # retrieve the inverse of the matrix, which should be NULL
cacheSolve(myMatVec)                # solve & return the inverse of the matrix
myMatVec$getinverse()               # retrieve the inverse of the matrix
myMatVec$set(matrix(5:8, nrow = 2)) # reset matrix with new values
myMatVec$get()                      # retrieve the new matrix
myMatVec$getinverse()               # retrieve the inverse of the new matrix, which should be NULL
cacheSolve(myMatVec)                # solve & return the inverse of the new matrix
myMatVec$getinverse()               # retrieve the inverse of the new matrix


