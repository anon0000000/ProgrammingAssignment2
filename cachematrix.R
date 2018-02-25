## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL ##sets m to null internal to function and assigned default value
    set<-function(y){
      x<<-y ##sets x to y in/outside of current environment so cache solve can see if data exists
      m<<-NULL ##sets m to null both in/outside of current environment
    }
    
    get<-function() x 
    setmatrix<-function(solve) 
      m<<-solve 
    getmatrix<-function() m 
    
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) ##list containing functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  ##checks to see whether m is null as condition for if function
  if(!is.null(m)) {
    message("Pulled from cache") ##informs user data is returned from cache when m is not NULL
    return(m)
  }
  
  matrix<- x$get()
  m<-solve(matrix,...) ##assigns return of solve function to m which is the inverse figures 
  x$setmatrix(m) ##sets cached data
  m        ## Return a matrix that is the inverse of 'x'
}
