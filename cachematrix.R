## The two functions makeCacheMatrix and cacheSolve, caches the inverse of the matrix
## makeCacheMatrix creates a special matrix object which is actually a list of functions

makeCacheMatrix <- function (x=matrix())
{
  m<-NULL
  
  ## set : to set value of matrix
  set <- function(y)
  {
    x<<-y
    m<<-NULL
  }
  ## get : to get value of matrix
  get <-function()
  { 
    x
  }
  ## setMatInverse: to set inverse of a matrix  
  setMatInverse<- function(Invers)
  {
    m<<-Invers
  }
  
  ## getMatInverse: to get inverse of a matrix 
  getMatInverse<-function()
  {
    m
  }
  ##returns the list of functions
  list(set=set, get=get, setMatInverse=setMatInverse,getMatInverse=getMatInverse)
  
}

## cacheSolve function calculates the inverse of a matrix. It first checks if the inverse is
## already calculated. If it is not, then it computes the inverse. If it is, then it reads 
## inverse from cache.

cacheSolve <- function(x, ...) {
  m <- x$getMatInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  m <- solve(data)
  x$setMatInverse(m)
  m
}

