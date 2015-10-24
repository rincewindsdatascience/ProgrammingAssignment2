## Two functions that creates a matrix, computes its inverse, and then stores the inverse
## for further usage.  

## Creates a list that sets the value of the matrix, gets the value of the matrix, sets the value of the inverse, 
## and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set the local variable m to NULL
  m <- NULL
  #create local function set, can be accessed with $ operator
  set <- function(y) {
    #sets the variable x equal to y; x is not local to this function, but the function 'above' variable
    x <<- y
    #set the global variable m to NULL
    m <<- NULL
  }
  #create a local function that returns the matrix x
  get <- function() x
  #set the inverse of the matrix m, called by $ operator
  setinverse <- function(solve) m <<- solve
  #returns inverse computed in the above line, called by $ operator
  getinverse <- function()m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #sets the local variable m to the inverse of the global variable x that
  #was created in the function makeCacheMatrix
  #The function 'solve' is not applied to x when calling makeCacheMatrix.
  #The first time calling cacheSolve on a matrix will return a getinverse as NULL because it has not
  #been set yet by setinverse.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  #get the matrix
  data <- x$get()
  #compute the inverse
  m <- solve(data)
  #set the inverse in the variable m
  x$setinverse(m)
  #return the inverse
  m
}
