## These functions will allow you to invert a matrix and store it in a 
## different environment, retrieving it later for use more quickly and
## easily than computing the entire inversion again.
##
## Once you have created a matrix, call makeCacheMatrix on it and store
## this as a new variable. If you look at your new variable, you will
## see there is nothing there - you haven't solved it yet! Now call 
## cacheSolve on your new variable.
##
## Once the matrix has been inverted once, it is stored in the cache.
## Assuming there are no changes to the matrix, any future calls to
## cacheSolve will retrieve the inverted matrix from the cache, rather
## than compute it all over again.

## This is the function that stores the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolution <- function(solve) m <<- solve
      getsolution <- function() m
      list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
}

## This function first looks for the matrix in getsolution, which is defined
## in the above function. If a NULL matrix is returned (meaning it hasn't been
## inverted yet), cacheSolve calculates the inversion. If it is not NULL
## (meaning it has already been computed), cacheSolve simply retrieves the
## inverted matrix.

cacheSolve <- function(x, ...) {
      m <- x$getsolution()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolution(m)
      m
}