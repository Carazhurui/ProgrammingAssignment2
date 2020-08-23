## This function creates a special "matrix" object that can cache its inverse.

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inversed matrix
## 4.get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inver <<- inverse 
  getinver <- function() inver
  list(set = set, get= get, setinver = setinver, getinver = getinver)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## 1.check to see if the matrix has already been inversed
## 2.inverse the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver < x$getinver()
  if(!is.null(inver)){
    message('getting inversed data')
    return(inver)
  }
  data <- x$get()
  inver <- solve(data,...)
  x$setinver(inver)
  inver
}
