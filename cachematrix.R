## cacheMatrix.R contains the following two functions
## 
## 1. makeCacheMatrix : makeCacheMatrix is used to cache a matrix, cache the 
##                      inverse of a matrix, retrieve cached matrix and 
##                      retrieve cached inverse of a matrix.
## 
## 2. cacheSolveMatrix : cacheSolveMatrix is used to calculate the inverse of 
##                       a cached matrix and if the inversed matrix is 
##                       not cached, it also caches the inversed matrix.


## makeCacheMatrix function caches the matrix and the inverse of a matrix
## and has get methods to retrieve matrix and inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse matrix 'inv'  to NULL
        inv <- NULL
        
        ## set the value of the 'x' and also set inverse matrix 'inv' 
        ## to NULL in the same environment as function makeCacheMatrix
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }
        
        ## function to get the value of 'x' - this returns a matrix 
        get <- function() x
        
        ## sets the inverse matrix 'inv' with passed 'invmatrix argument'
        setInverse <- function(invmatrix){
                inv <<- invmatrix
        }
        
        ## get the inverse matrix 'inv' 
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function is used to calculate the inverse of a matrix. 
## If the inverse matrix is already calculated,
## function simply retrieves the value from the cached object instead of
## re-calculating

cacheSolve <- function(x, ...) {
        
        ## get the cached inverse matrix
        invmat <- x$getInverse()
        
        ## check if the 'invmat' is not null and if it is not null, return the cached
        ## inverse matrix
        if(!is.null(invmat)){
                ## getting cached inverse matrix
                return(invmat)
        }
        
        ## if 'invmat' is null, then get the cached matrix
        mat <- x$get()
        
        ##calculate the inverse of the cached matrix
        invmat <- solve(mat)
        
        ## set the inverse matrix to be cached 
        x$setInverse(invmat)
        
        ## return the inverse of the matrix 'x' as result
        invmat
}
