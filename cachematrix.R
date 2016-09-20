## The makeCacheMatrix is seting the matrix, getting the matrix
## setting the inverse of the matrix and getting the inverse of the
## matrix. 


## the function take a matrix, x should be matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # solve is the function to get the inverse of a full rank matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  # have the get and set
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that takes the format of makeCacheMatrix(x)
## and it will return the inverse matrix of x.


# x is the output of the makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached mymatrix")
    return(m)
  }
  mymatrix <- x$get()
  m <- solve(mymatrix, ...)
  x$setinverse(m)
  m
}

## testing code if need
# x <- matrix(c(1,3,4,2,5,7,8,6,9),3,3)
# solve(x)
# > solve(x)
# [,1] [,2] [,3]
# [1,]  0.6  7.6 -5.6
# [2,] -0.6 -4.6  3.6
# [3,]  0.2  0.2 -0.2
# makeCacheMatrix(x)
# cacheSolve(makeCacheMatrix(x)) 
# > cacheSolve(makeCacheMatrix(x))
# [,1] [,2] [,3]
# [1,]  0.6  7.6 -5.6
# [2,] -0.6 -4.6  3.6
# [3,]  0.2  0.2 -0.2