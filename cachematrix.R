## The below pair of functions caches the inverse of a matrix

## Creating a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##setting default value if cacheSolve is not used
 
  set <- function(y){ ##setting the value of the matrix
    x <<- y 
    i <<- NULL
  }
 
  get <- function() x  ## returns x
  
  setinverse <- function(solve) i <<- solve ##inverse of a square matrix can be done with the solve function
  
  getinverse <- function() i  ## returns inverse matrix, i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## returns the 'special vector' containing all of the functions just defined
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## i can be NULL (or) gets the inverse matrix if the inverse has been calculated already
        
        if(!is.null(i)){ ## check if this cacheSolve is run before
          message("getting cached data")
          return(i) ## returns cached inverse matrix 
        }
        
        ##Otherwise
        mat <- x$get() ## assign the matrix to mat
        i <- solve(mat, ...) ## calculate the inverse of mat and assign to i
        x$setinverse(i) ## set inverse matrix to x, so that it can be cached if necessary
        
        i ## returns uncached inverse matrix
}
