## The following functions implement a matrix inverse object.  A matrix multiplied by its inverse matrix
## results in an identity matrix (1s on diagonal, 0s otherwise).  The first attempt to invert a matrix
## calcultes its inverse.  As long as the original matrix does not change, the inverse is not recomputed,
## saving computation time.
## 
## Test Data
## a <- matrix(1:4,2,2)
## b <- matrix(rnorm(9),3,3)
## c <- matrix(rnorm(16),4,4)
##
## Test Procedure (where x = a, b, and c)
## 1. x %*% cacheSolve(makeCacheMatrix(x))      ## printed an identity matrix of the appropriate dimension
## 2. cacheSolve(makeCacheMatrix(x)) %*% x      ## printed an identity matrix of the appropriate dimension
## 3. packageStartupMessage("starting...")
##    y <- makeCacheMatrix(x)
##    cacheSolve(y)                             ## No "getting cached data" message printed
##    cacheSolve(y)                             ## "getting cached data" message printed
##    cacheSolve(y)                             ## "getting cached data" message printed
##    cacheSolve(y)                             ## "getting cached data" message printed
##    ...

## Returns a matrix whose inverse will only be calculated once.
makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL                           ## Initially nullify the inverse matrix
        set <- function(y) {                    ## Save original matrix and nullify its inverse
                x <<- y
                x.inv <<- NULL
        }
        get <- function() x                     ## Return the original matrix
        setInv <- function(xi) x.inv <<- xi     ## Save the inverted matrix
        getInv <- function() x.inv              ## Return the inverted matrix
        list(set = set, get = get,              ## List of functions provided
             setInv = setInv,
             getInv = getInv)
}


## Calculates and saves the inverse of a cachedMatrix object.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()                       ## Fetch the calculated copy of the inverse
        if(!is.null(inv)) {                     ## If previously calculated, return that value
                message("getting cached data")  ## Print message on invocations where cached value was returned
                return(inv)
        }
        data <- x$get()                         ## Otherwise, fetch the original matrix
        inv <- solve(data, ...)                 ## Calculate its inverse
        x$setInv(inv)                           ## Save it for subsequent calls to cacheSolve()
        inv                                     ## Return the inverse matrix
}
