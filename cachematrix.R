# This function returns the inverse of a matrix, but before
# computing the inverse, it checkes to see it if has been computed before.
# If it has, it gets the results from memory (which saves time)
#    
# ***********************************************************
#

#The first function, makeMatrix creates a special "matrix", which is really 
# a list containing a function to

#set the value of the matrix to be inverted
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## this function solves for the inverse of the matrix
# but first, it checks if the matrix has already been inverted
# it does this bye checking the corresponding element in the list
# created by the function MakeMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inverse<- solve(data, ...)
  x$setinvere(m)
  m
}
