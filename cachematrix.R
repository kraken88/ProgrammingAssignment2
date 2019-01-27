## The following function create a matrix that can cache its inverse

## We will create a matrix which is a list containing a function to 
## set and get the values

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL         ##Setting the value of inv to null
  set = function(y)  ##Setting the value of the matrix
  {
    x <<- y          ##Caches the input matrix
    inv <<- NULL
  }
  
  get = function()x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function calculate the inverse of a matrix. If the inverse
## has been calculated the value will be retrieve from the cache

cacheSolve <- function(x,...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## Checcking if the inverse has already been calculated. If not, 
  ## it calculate the value
  if(!is.null(inv))
  {
    return(inv)
  }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}