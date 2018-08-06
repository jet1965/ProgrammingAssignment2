## The functions definwed below work in tandem. They are 
## sdirectly derived (hacked) from the example code functions, 
## makeVector and cacheMean.
## Like those two functions, makeCacheMatrix creates a vector 
## that is not a numeric vector, but a vector of functions. 
## These are used by the cachesolve function to create 
## an inverse matrix, cache that inverted matrix, and retrieve 
## the cached matrix in lieu of creating it, if it exists.

#############################################################
## To use these two functions, first build a matrix like this:
##
## > mx <- matrix( c(1, 2, 3, 4, 5, 6), 2, 3)
##
## Now you can call makeCacheMatrix with this object:
##
## > mx2 <- makeCacheMatrix(mx)
##
## mx2 can now be used to call cachesolve. Recall that 
## mx2 is not a matrix, like mx, but a vector of functions 
## and data. So when you enter the above function call the 
## first time it simply returns the transposed matrix.
##
##  1  3  5   becomes  1  2
##  2  4  6            3  4
##                     5  6
##
## If you run this call again, however, the print message 
## from the TRUE side of the conditional informs you that 
## the cached transposed matrix was retrieved, rather than
## calculated.
#############################################################

## makeCacheMatrix allocates memory and defines function 
## stubs to store, hold, and retrieve the cached, transposed 
## matrix input by the user.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix){ 
    m <<- matrix
  }
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cachesolve retrieves whatever is stored at the 
## memory location specified by makeCacheMatrix. If a 
## cached matrix is already stored there, it returns this 
## r object rather than computing it again. If the 
## x$getmatrix() call returns NULL to m, the matrix has 
## not yet been computed or cached and the remainder of 
## cachesolve takes the matrix and returns its 
## transpose.

cachesolve <- function(x, ...) {
  ## Retrieve the matrix stored by makeCacheMatrix.
  ## If there is data stored here, retrieve the cached 
  ## transpose. Otherwise, calculate it. < t(data) >
  ## Then cache t(data) via the setmatrix() function.
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- t(data)
  x$setmatrix(m)
  m
}
