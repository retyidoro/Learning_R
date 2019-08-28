#saves the matrix and its inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
  
  inv <<- NULL
  
  #setting ut pthe matrix values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #getting the matrix values
  get <- function() x
  
  #setting the matrix inverse
  setinv <- function(i) inv <<- i
  
  #getting the inverse
  getinv <- function() inv
  
  #return the list
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## calculating the inverse of a matrix, if it is not calculated already

cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("the inverse was cached")
    return(inv)
  }
  data <- x$get() #getting the matrix
  inv <- solve(data)
  x$setinv(inv)
  inv
}
