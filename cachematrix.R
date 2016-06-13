makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  #--- set the matrix ---
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  # ---  get the matrix ----
  get <- function() 
  {
    x
  }
  #--- set the inverse of the matrix----
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  #---get the inverse of the matrix ----
  getInverse <- function()
  {
    m
  }
  #-- Return list of methods ---
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## This function caches the inverse of a matrix

cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  #--- if inverse is set already, return it ---
  if( !is.null(m) )
  {
    message("getting cached data")
    return(m)
  }
  #--- Get the matrix from our object ---
  data <- x$get()
  #--- inverse calculation using matrix multiplication ---
  m <- solve(data) %*% data
  #--- Set the inverse to the object ---
  x$setInverse(m)
  #--- Return the matrix---
  m   
}

