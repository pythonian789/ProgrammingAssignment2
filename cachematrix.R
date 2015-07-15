#These two functions provide a way to compute and temporarily 
#store (also called cache) the inverse of a matrix.

#this function creates a list of functions that solve, store, and return the matrix when called
makeCacheMatrix <- function(x=matrix()) {
      i <- NULL
      set <- function(y) {
            #the "<<-" operator makes x into a type of glabal variable
            #that can be accessed outside of the function
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#This function takes a matrix as its input, checks to see if it's already been solved,
#returns the solution if it has, and solves the matrix if it hasn't.
cacheSolve <- function(x, ...) {
      i < x$getinverse()
      #This if statement chacks to see if the matrix has been solved already
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      i$setinverse(i)
      i
}