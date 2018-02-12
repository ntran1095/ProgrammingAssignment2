## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeInverse <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        
    
}

#Testing
x1 <- makeInverse(matrix(c(1,2,3,4),2,2))
x1$getinverse() #Inverse not computed yet
cachesolve(x1) ##inverse returned after computation
cachesolve(x1) ##inverse returned from cache

x1$set(x1$getinverse()) ##Setting the function call to be the computed inverse
cachesolve(x1)  ##Inverse of the inverse is the original matrix