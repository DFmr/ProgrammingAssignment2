##the following 2 functions (i.e. makeCacheMatrix and cacheSolve) return
##the inverse of a function

##If the inverse of the matrix already exists, the functions will return
##the previously calculated inverse

##if the inverse of the matrix has not been previously calculated, the functions
##will calculate the inverse of the function and return results

## the makeCacheMartix function creates a vector consisting of a list that does 4 things:
# 1 sets up a matrix (i.e. set)
# 2 gets the values  within a matrix (i.e. get)
# 3 sets up the inverse of the matrix (i.e. setinverse)
# 4 gets the values for the matrix inverse (i.e. getinverse)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  x<-x
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve checks to see if the inverse of the matrix created above has already
##been calculated

##if the inverse has been calculated, the function returns the inverse values

##if the inverse has not been calculted, the function uses the solve function to 
##calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m    
}
