## makeCacheMatrix contains 4 functions. These functions namely deal with matrix x, storing inverse matrix m and cache it.
## By calling the set function the inverse matrix m is erased and after calculation replaced by new value


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL          #initialize the inverse property
  set <- function(y) {          #setting the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x           #getting the matrix
  setinverse <- function(inverse) m <<- inverse        #setting the inverse of the matrix
  getinverse <- function() m                           #getting the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    #return a list of the methods
}


## Calculates the inverse of the square matrix or retrieves a previously calculated inverse from cache.However, it first checks to see if the inverse has already been calculated.If so, it gets the inverse from the cache and skips the computation.Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()               #get the matrix from our object
  m <- solve(data, ...)         #calculate the inverse 
  x$setinverse(m)
  m
}



