## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #initialize an NULL "inverse matrix"
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x #get the original matrix
    setinv <- function(sinv) inv <<- sinv #this will update the initially NULL inv value with the calculated inverse matrix
    getinv <- function() inv #this returns the inv value stored in the cache (if cacheSolve has never been run then this will return a NULL value)
    list(set=set, get = get, setinv = setinv, getinv = getinv)
}



## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated & the matrix is
## the same, then the function cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() #access the inv value stored in the cache in makeCacheMatrix
    if (!is.null(inv)){ #if the inverse has been calculated before, it will return the inv without further computation
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #getting the original matrix
    inv <- solve(data, ...) #solving for the inverse
    x$setinv(inv) #inputting the result(solved inverse) into the cache
    inv #return the inverse
}

## THE END