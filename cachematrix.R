## Basic function of this program is to reduce the computation time
## of those calculations where same data is processed many times.

## In makeCacheMatrix function, I have fixed the environment for 
## input matrix(x) and its inverse(i).

## Initially inverse matrix(i) is set to NULL so as to hold only
## place without any value.

## In the end all functions are included in a list and given names
## so that they can be accessed from outside of makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix())
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Main function of cacheSolve is to first check the value of
## inverse in Cache and retreive it if it is not NULL.

## In case value of Inverse Matrix(i) is NULL then it computes
## it and set its value in the Cache

## In the end it returns the inverse of input matrix

cacheSolve <- function(x, ...)
{
  i <- x$getInverse()
  if(!is.null(i))
  {
    message('Retreiving cache data')
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
