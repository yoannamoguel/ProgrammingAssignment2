## @x: a square invertible matrix
## return: a list containing functions to
## i.  set the matrix
## ii. get the matrix
## iii.set the inverse of the matrix
## iv. get the inverse of the matrix
## the list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set= function (y) {
    ## "<-" is used to assign a value to an object in a different enviroment
    ## from the current one.  
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv <- function(inverse) 
    inv<<-inverse
  getinv <- function () inv
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}

## The cacheSolve function returns a matrix that is the inverse of 'x'
## x-> output of makeCacheMatrix()
## return-> inverse of the original matrix which is inputinto makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  inv= x$getinv()
  ##check if the inverse has already been calculated
  if(!is.null(inv)){
    ##get it from the cache and skips the computation
    message("getting cached data")
    return(inv)
  }
  ##otherwise, calculates the inverse matrix 
  mat.data = x$get()
  inv= solve(mat.data,...)
  ##sets the value of the inverse matrix in the cache 
  ##via the setinvfunction
  x$setinv(inv)
  return(inv)
}
