## The following function calculates the inverse of the special "matrix" created with the functions. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the solve function.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-null
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()
    setinv<-function(solve) m<<-solve
  getinv<<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.na(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}
