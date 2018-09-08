## Caching the matrix

## This function caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y)
  {
   x<<-y
   i<<-NULL
  }
  get=function()x
  setinv=function(inverse)i<<-inverse
  getinv=function();
  list(set=set,get=get,setinv=inv,getinv=inv)

}

## This function returns inverse of the matrix after caching

cacheSolve <- function(x, ...) {
   i=x$getinv()
   if(!is.null(i))
   {
    message("Getting Cached Data")
    return(i)
   }
   data<-x$get()
   i=solve(data,...)
   x$getinv(i)
   i     
}
