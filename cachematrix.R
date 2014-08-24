## The functions create a matrix object to cache the results of the inverse of the input matrix.


## Function to set/get the values of the matrix and set/get the values of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(m){
    x<<-m
    i<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) i<<-inverse
  getInverse<-function() i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## checks for the cached inverse of the matrix and returns the inverse. if not found, calcultes the
## inverse of the matrix, sets its inverse in the chache and returns the inverse.

cacheSolve <- function(x, ...) {
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cashed data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setInverse(i)
  i
}
