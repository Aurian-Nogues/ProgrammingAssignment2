
#makeCacheMatrix takes an invertible matrix as an argument and returns an object containing various variables and functions to be used in cacheSolve
#cacheSolve takes an object created by makeCacheMatrix as an argument. It takes the original matrix from the objects, calculate it's inverse, store it in the object and return it.
#if cacheSolve is called after the inverse of the matrix has been calculated it will skip the computation part and extract the inverse from the object

# //////// workflow: ////////

# x <- matrix(rexp(100), 10) # generates 10x10 matrix of random numbers
# xObj <- makeCacheMatrix(x) #create the object to be used in cacheSOlve
#cacheSolve(xObj) #calculate x inverse, store it in xObj and returns it
#cacheSolve(xObj) #extract x inverse from xObj and returns it

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  
  setx <- function(y) {
    #set value of original matrix and clear inverted
    x <<- y
    inverted <<- NULL
  }
  
  getx <- function() x #returns value of x
  setInverted <- function(inverted) inverted <<- inverted #sets value for inverted matrix
  getInverted <- function() inverted #returns value for inverted
  
  #write all functions and variables in list
  list(x = x, inverted = inverted,setx = setx, getx = getx, setInverted = setInverted, getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getInverted()
  if (!is.null(inverted)) {
    message('recovering inverted matrix from cache')
    return(inverted)
  } else {
    message('computing inverted matrix and storing it in cache')
    original <- x$getx()
    inverted <- solve(original)
    x$setInverted(inverted)
    message('Inverted matrix:')
    return(inverted)
  }
  
}
