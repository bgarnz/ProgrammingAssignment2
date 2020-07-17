## makeCacheMatrix creates a special object containing a matrix with
## the purpose of being able to solve for its inversion. If it has 
## already been solved in the past, it can just retrieve the inversion
## instead of solving again.

## makeCacheMatrix creates an object that contains a matrix and 4
## different functions that can be called: set, get, setInv, and 
## getInv. get, setInv, and getInv are only meant to be used in 
## the other function.

makeCacheMatrix <- function(thisMatrix = matrix()) {
        matInv <- NULL
        set <- function(newMatrix) {
                thisMatrix <<- newMatrix
                matInv <<- NULL
        }
        get <- function() thisMatrix
        setInv <- function(newInverse) matInv <<- newInverse
        getInv <- function() matInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Solves for the inversion of a matrix using the special object
## created by makeCacheMatrix.

cacheSolve <- function(cacheMat, ...) {
        ## Return a matrix that is the inverse of 'cacheMat'
        matInv <- cacheMat$getInv()
        if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        data <- cacheMat$get()
        matInv <- solve(data, ...)
        cacheMat$setInv(matInv)
        matInv
}
