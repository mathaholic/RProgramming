makeCacheMatrix <- function(x = matrix()) {
  ##this makes my constructor function for the cache
  ##it tells R to look in the created environment to find 
  ##the inverse of a matrix instead of computing it
  ##each time its called
  
  m <- matrix(, nrow=0, ncol=0)
  set <- function(y){
    x<<-y
    m<<-matrix(, nrow=0, ncol=0)
    ##This defines our matrix "x" in the parebt environment
    ##and not just the environment of the function
    ##The Null Matrix M is defined for use later
  }
  
  get <- function() x
  ## a primitve function for later use
  
  setinv <- function(solve) m <<- solve
  ## find the inverse of a matrix and redefine m as the inverse
  ##note m is defined in the parent environment and not just
  ##in the function's environement.
  
  getinv <- function() m
  ##a primitive function that will return the inverse of x
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  ##shows the programmer each element of makeCacheMatrix
  ## and in which environment it lives
  
}

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    ##this checks to see if our matrix x is the null matrix
    ## The null matrix's inverse is undefined because its 
    ## determinant is zero, and we cannot divide by zero
  }
  data <-x$get()
  ##defines the x matrix within the local environment
  m<-solve(data)
  ##defines the inverse matrix of the previous line
  x$setinv(m)
  ## saves the inverse matrix in the cache
  m
  ##prints the inverse
}