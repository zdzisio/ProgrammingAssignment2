
## makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse
## it is list containing function to
##  1.  set the matrix
##  2.  get the martix
##  3.  set the inverse matrix
##  4.  get the inverse matrix 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## function calculates the inverse of the special "Matrix"
## created with "makeCacheMatrix" function. 
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the Inverse in the cache via the `setsolve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

