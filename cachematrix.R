## The finctions makeCacheMAtrix and cacheSolve check whetther the inverse of a given matrix has
## already been calculated and if not, calculate it

## makeCacheMatrix stores 4 functions that return the matrix stored in main function (get), 
## change it (set), and similarly return the value of input variable m (getinv) and store it 
## into the main function


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix, stored in MakeCacheMAtrix object. If it's 
## already calculated, chacheSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
