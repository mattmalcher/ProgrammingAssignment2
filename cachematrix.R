## M. Malcher 29/07/17 
## Code for programming exercise: Learning how to use the <<- operator in R to
## take advantage of lexical scoping to cache computationally expensive results.

# Useful Reference! 
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL           # Set the initial value of inverse matrix as NULL
  
  set<- function(y){      # Create a set function, with which to set the value of x (input matrix)
    x <<- y               # This can set x to an arbitrary variable
    inv_x <<- NULL        # It also resets the cache. :)
  }
  
  get <- function() {
    x                     # Create a function to retrieve the input matrix x from the object we are creating
  }
  
  set_inv <-function(inverse) {
    inv_x <<- inverse     # Create a function to set the inv_x (cache) as the input "inverse"
  }
  
  get_inv <-function() {
    inv_x                 # Create a function to retrieve the cached inverse
  }
  
  list(set=set, #creates a list of the functions making them accesible as x$functionname
       get=get,
       get_inv=get_inv,
       set_inv=set_inv)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$get_inv()                  #Get inverse currently stored in object (null or cached)
  
  if (!is.null(inv_x)){                 # If it isnt null
    message("getting cached inverse")
    return(inv_x)                       # return the cached value :)
  }
  
  data <- x$get()                       # If it IS null then we want to get the input data to calculate the inverse

  inv_x <- solve(data)                  # Do the hard bit (invert the matrix)
  x$set_inv(inv_x)                      # Use the function we put in the object to set the cache to our solution
  
  inv_x                                 # return the inverse (as just calculated & cached)
  
}
