## Overall description: "makeCacheMatrix" will first create a special
## "matrix" object, which is really a list containing a function to
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the inverse of the matrix.
## $. get the inverse of the matrix.
## "makeCacheMatix" can cache its inverse. "cacheSolve" will first check
## the cache to see if the inverse of the unchanged "matrix" already exists(i.e.
## is not null). If true, it will return the cached inverse. If false, 
## it will calculate the inverse and return it.

## "makeCacheMatrix" creates a special "matrix" object (a list) that can cache 
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)j<<-inverse
  getInverse<-function()j
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## "cacheSolve" will now calculate the inverse of a matrix by first checking
## for the existence of a cached solution returned by "makeCacheMatrix". If 
## no cached solution is found, it will solve for the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  j<- x$inverse()
  if (!is.null(j)){
    message("getting cached data")
    return(j)
  }
  data<- x$get()
  j<- solve(data,...)
  x$setInverse(j)
  j
}
