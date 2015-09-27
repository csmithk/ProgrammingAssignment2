## makeCacheMatrix and cacheSolve matrix work to provide functionality to cache the inverse of a 
## matrix in a separate environment to avoid the costly computation of calculating the inverse of a matrix

##makeCacheMatrix creates a special matrix object that can cache it's inverse
## it is comprised of functions: set, get, setInverse and getInverse 
makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    #set will set inverse back to null, so if it changes, will remove from cache
    set <- function(y = matrix()){
      x <<- y
      m <<- NULL
    }
    
    get <- function() { x }
    
    setInverse <- function(inverse) {
      
        m <<- inverse
    }
    
    getInverse <- function () m
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## returns the cached inverse if it's already cached, otherwise creates and caches the inverse
cacheSolve <- function(x ) {
    data <- x$get()
    m <- x$getInverse()
    #if it exists in cache and is not null
    if(!is.null(m)){
      message("getting cached data")
      return (m)
    }
    
    m <- solve(data)
    x$setInverse(m)
    
    m
}


