##The first function creates a matrix that can be inverted.  From there we can cache said inverse.
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following will calculate the inverse of the matrix from the above function. If we have 
## already computed the inverse of the matrix, this will retrieve the inverse from the cache.
cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}