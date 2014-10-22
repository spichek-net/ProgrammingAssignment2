## Application of pattern given in "Example: Caching the Mean of a Vector"
## to caching matrix inversion assignment 


## The function creates
## 1) environment that holds reference to matrix and optional reference to inverse of it
## 2) functions to access the above references
## 3) functions to mutate the references
## Then it returnes references to the functions in a list with named elements

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## The function
## (1) goes to cache for previosly computed value
## (2) if cache is empty, calculates the inverse of matrix, and puts it in cache
## (3) returns the inverse

cacheSolve <- function(x, ...) {

  i <- x$getInv()

  if(is.null(i)) x$setInv( i <- solve(x$get(),...) )
  else message("getting cached data") ## <else> can be omitted, only useful for illustration
    
  i ## Return a matrix that is the inverse of 'x'
}

## Most likely following alternative would save some space in the environment, arguably, without loss of readbility

makeCacheMatrix2 <- function(x = matrix()) {
  i <- NULL
  list(set =    function(val) { x <<- val; i <<- NULL; },
       setInv = function(inv) { i <<- inv },
       get =    function() x,
       getInv = function() i
  )
}

## Example
## source("cachematrix.R")
## m <- matrix(c(1,2,3,4), ncol=2)
## c1 <- makeCacheMatrix(m)
## i1 <- cacheSolve(c1)
## i2 <- cacheSolve(c1)
## e1 <- identical(i1,i2)
## v <- solve(m)
## e2 <- identical(i1,v)
 
## c2 <- makeCacheMatrix2(m)
## i3 <- cacheSolve(c2)
## i4 <- cacheSolve(c2)
## e3 <- identical(i3,i4)
## e4 <- identical(i1,i3)
## message(paste("All equal:", as.logical(prod(e1,e2,e3,e4))))
