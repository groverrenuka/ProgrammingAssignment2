## The following two functions are used to cache the inverse of a matrix.
## This helps in cost savings by avoiding repeated computations.

## Function 'makeCacheMatrix' creates a "special matrix" object that caches the inverse
## of a matrix. It has four functions to set, get value of matrix and to set get inverse 
## matrix respectively.

makeCacheMatrix <- function (x=matrix())
{
  m<-NULL
  
  ## sets value of matrix
  set <- function(y)
  {
    x<<-y
    m<<-NULL
  }
  ## gets the value of matrix
  get <-function()
  { 
    x
  }
  ## Sets the inverse of a matrix  
  setMatInverse<- function(Invers)
  {
    m<<-Invers
  }
  
  ## Gets the inverse of a matrix 
  getMatInverse<-function()
  {
    m
  }
  ##returns the list of functions
  list(set=set, get=get, setMatInverse=setMatInverse,getMatInverse=getMatInverse)
  
}

## Function 'cacheSolve' calculates the inverse of the "special matrix". 
## It first checks if the inverse is already calculated. If it is not, then it computes
## the inverse and sets it into the cache. If it is, then it reads the inverse from cache 
## and skips the calculation.

cacheSolve <- function(x, ...) 
{
    m <- x$getMatInverse()
    
    ## checks if inverse is already calculated
    if(!is.null(m)) 
    {
      message("getting cached data")
      return(m)
    }
   ## if inverse is not calculated, get the matrix and compute the inverse
  data <- x$get()
  m <- solve(data)
  
  ##set the outcome into cache
  x$setMatInverse(m)
  m
}

