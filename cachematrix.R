## These functions take a matrix and calculates its inverse if the solution
## has not already been cached otherwise it returns the cached inverse matrix.

## Function to create a special matrix object that can cache its inverse and
## has functions to store and get the matrix and also to set the inverse
## value and get the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function calculates the inverse of the special matrix created by the 
## makeCacheMatrix function after checking whether the inverse has already
## been calculated. If so, it will retrieve the cached inverse matrix 
## otherwise it computes the matrix's inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        data <-  x$get()
        # Find function to make inverse of matrix
        i <- solve(data, ...)
        x$setInv(i)
        i
}
