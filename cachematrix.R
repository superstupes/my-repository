## Put comments here that give an overall description of what your
## functions do
## The first function stores the inverses of matrices and the latter function
## accesses and returns the relevant inverse if the relevant data to the input is stored.

## Write a short comment describing this function
## This function stores inverses of the matrices and makes it possible for
## the other function to get the cached data
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) inv<<- solve
  getmatrix<-function() inv
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
## This function gets the stored data if the inverse of the input matrix is cached
## and returns the inverse without calculating it every time.
cacheSolve <- function(x, ...) {
  inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
