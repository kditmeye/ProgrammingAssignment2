## these function calculate the inverse of a matrix in a cache

## this function create a cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   #begins by setting the inverse to NULL as a placeholder for a future value
  #defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, i, to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x   #returns the matrix, x
  setinverse <- function(inverse) i <<- inverse #sets the inverse, i, to inverse
  getinverse <- function() i  #returns the inverse, i
  #returns the 'special vector' containing all of the functions just defined.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this fonction calculate the inverse of the matrix 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()  #assign to i in cacheSolve, the value from getinverse(i) that is NULL.
  #If the inverse is stored under the parameters "matrix x" is not NULL, return it.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #If the inverse is not stored then:
  data <- x$get() # assign to data the vector x
  i <- solve(data) #Calculate the inverse and assign it to i
  x$setinverse(i) #Store the inverse "i" under the parameters "matrix x".
  i #Return i
}
