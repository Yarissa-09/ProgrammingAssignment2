## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize symbol for cached inverse of matrix
    inv <- NULL
    # Initialize symbol to indicate matrix has changed
    matChanged <- TRUE

    # Set a matrix in the data structure
    setMatrix <- function(x) {
        m <<- x
        inv <<- NULL
        matChanged <<- TRUE
    }

    # Get the currently set matrix
    getMatrix <- function() m

    # Set the calculated inverse of a matrix, 
    # and indicate that the matrix is not changed
    setMatInverse <- function(inverse) {
        matChanged <<- FALSE
        inv <<- inverse
    }

    # Get the inverse of the matrix
    getMatInverse <- function() inv

    # Returns whether the matrix has changed or not
    isMatChanged <- function() matChanged

    # Return the list of functions that can be executed
    # on this data structure
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatInverse = setMatInverse,
         getMatInverse = getMatInverse,
         isMatChanged = isMatChanged)
}


## Write a short comment describing this function - Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         # Get the cached inverse of matrix
    inv <- specialMatrix$getMatInverse()
    # Ask whether the matrix has changed or not
    isMChanged <- specialMatrix$isMatChanged()

    # If the cached inverse is not NULL and
    # if the matrix has not changed
    if(!is.null(inv) && !isMChanged) {
        message("Get cached inverse of matrix")
        return(inv)
    }

    # Get the current matrix for which inverse 
    # has been requested
    m <- specialMatrix$getMatrix()

    # Calculate the inverse of matrix
    inv <- solve(m, ...)
    # Set the calculated inverse in data structure
    specialMatrix$setMatInverse(inv)

    message("Get calculated inverse of matrix")
    inv
}
