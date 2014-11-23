## The function makeCacheMatrix generates a special matrix object that can store
## the matrix itself and the inverse of the matrix. The function cacheSolve takes in
## that special matrix object, returns the inverse if the inverse is cached, otherwise 
## calculates the inverse, returns it and stores it in the special matrix object.

## makeCacheMatrix is a function that stores a matrix in x and its inverse in i.
## It returns a list with the following methods:
##      set(y) which sets the matrix
##      get() which returns the matrix
##      setInverse(inverse) which caches the inverse
##      getInverse() which returns the caching inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that takes in a special matrix object generated from 
## makeCacheMatrix, then check if the inverse is already calculated. If so, the 
## function returns the cached matrix; if not, it calcluates the inverse of the matrix,
## returns it and stores the inverse in the special matrix object

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}

cachedM = makeCacheMatrix(matrix(c(1,2,3,4),2,2))
cacheSolve(cachedM)

