## Creating a function makeCacheMatrix() to save a matrix that will be later 
##used to calculate its inverse with a function cacheSolve().



## x: matrix format vector that has to be determined by the user.
#example: Create a matrix and save in the function 
#mat <- matrix(c(4, 3, 3, 2), nrow = 2)
#saveMatrix <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(se =set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to calculate the inverse of the matrix created saved with makeCacheMatrix
# example: based on the matrix created with the first function: cacheSolve(saveMatrix)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message('getting cached data matrix')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}


