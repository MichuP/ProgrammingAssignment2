## Example matrix for testing: #b <-  matrix( c(2, 4, 3, 1, 5, 7, 6, 8, 9), nrow=3, ncol=3) 
## When testing, do the following:
## 1. test <- makeCacheMatrix(b)
## 2. cacheSolve(test) will calculate the inverse and then cache it
## 3. cacheSolve(test) again. This time you should get info comment that the value was retrieved from cash

## This function returns an interface for getting / setting the matrix as well as getting / setting the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function calculates the inverse or retrieves it from cache if calculated before

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting matrix inversion from cache")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
}
