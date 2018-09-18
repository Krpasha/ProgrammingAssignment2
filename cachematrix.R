## This set of functions work to store a square matrix ("M"), compute & cache 
## the inverse of this matrix ("matrix") as a special object within the function 
## (i.e. not global environment, allowing for faster computation

# FUNCTION: makeCacheMatrix

## makeCacheMatrix is a function which is actually a list containing 4 functions.   
## The first element is the set function, which sets the value of the matrix,
## the second element is the get funtion, which gets the value of the matrix,
## the third element is the setsolve function, which computes the inverse of the matrix
## the fourth element is the getsolve function, which gets the value of inverted matrix


makeCacheMatrix <- function(x = matrix()){
  M <- NULL
  
  set <- function (y) {
    x <<- y
    M <<- NULL
  }
  
  get <- function()x
  setsolve <- function (solve) M <<- matrix
  getsolve <- function ()M
  
  list (set = set, 
        get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

# FUNCTION: cacheSolve

## cacheSolve is a function that calculates the inverse of the matrix ("x") through 
## the above function (makeCacheMatrix), and sets the result "M" in the cache, 
## through the setSolve function.  But it first checks if the inverse was already calculated.
## if "M" was already calculated, it gets and returns the cached inverse matrix,
## skipping the rest of the computation. 

cacheSolve <- function(x, ...){
  M <- x$getsolve()
  if (!is.null(M)){
    message("getting cached data")
    return(M)
  }
  
  data <- x$get()
  M <- solve(data, ...)
  x$setsolve (M)
  M
}

