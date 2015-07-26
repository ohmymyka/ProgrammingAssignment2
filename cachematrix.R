## These 2 functions store into memory (cache) the inverse of a vector
## so that if it is needed in some other function, there
## will be no need to recompute it again.

## The makeCacheMatrix creates the special matrix m that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
              set <-function(y) {
                x<<- y
                m<<- NULL
              }
        get<-function() x
        setmat<-function(solve) m<<-solve(x)
        getmat<-function() m
        list(set=set, get=get, setmat=setmat, getmat=getmat)
  
}


## The cacheSolve function retrieves the inverse of the matrix if it has 
##already been calculated or computes the inverse of the matrix 
##if it hasn't been calculated yet.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmat()
              if(!is.null(m)){
                message("getting cached data")
                return(m)
                
              }
        data<-x$get()
        m<-solve(data)
        x$setmat(m)
        m
}
