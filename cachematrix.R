## Sien Verschave
## Caching the Inverse of a Matrix:
## ----------------------------------
## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix<-function(x = matrix()){
        inver <- NULL 
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from 
## the cache.
## Computing the inverse of a square matrix can be done with the 
## solve function in R. 
## For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("geting cached data")
                return(inver)
        }
        matrix <-x$get()
        inver <- solve(matrix, ...)
        x$setinverse(inver)
        inver
}