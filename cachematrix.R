## This two functions below do fasten the process of inversing a matrix  
## by taking advantage of the caching techinque

## makeCacheMAtrix does create a special matrix object that caches the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve does look into the cached matrix and if the current matrix is already cached, 
## returns the cached inverse matrix. If not, does compute the inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
