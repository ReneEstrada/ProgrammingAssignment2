## There are two functions to demonstrate the lexical scope
## of the R language.
## The assignment calls for a cached function to keep the latest 
## inverse cached version of an inverse matrix using the solve function
## makeCacheMatrix is to keep in cache the inverse matrix and the result of using 
## the solve function on the matrix
## and cacheSolve calls the makeCacheMatrix to see if a cached
## version of the inverse matrix is in memory, if not it will calculate
## and set in cache the new calculated value

## As per the example, this functions checks if a cached version
## of the matrix is there if not it sets and gets the value of the inverse
## matrix and also resolves the value of the inverse matrix and also
## sets the new value, it returns a list with the set and get functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cached solve takes makeCacheMatrix as parameter and it passes the
## value of the matrix to be inverted.
## it checks if the inverted value is in cache otherwise it proceeds
## to set the initialize the value of the matrix and it applies
## the solve function to get the new value and it sets in cache the 
## new result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Data in cache, it returns it")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
