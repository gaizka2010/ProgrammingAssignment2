##The first function, `makeCacheMatrix` does the following:

#1 set the value of the vector
#2 get the value of the vector
#3 set the value of the mean
#5 get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function returns the inverse of the matrix. If the 
# inverse has already been computed it gets the result and skips the
# computation. Otherwise, it computes the inverse and sets the value in 
# with the "setinverse" function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#Example

a <- c(2,3,4,5)
m <- matrix(a,2,2)

mI <- makeCacheMatrix(m)
cacheSolve(mI)

#Inverse returned directly from cache
cacheSolve(mI)






