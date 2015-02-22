## Functions makeCacheMatrix() and cacheSolve() 
## are used together to cache the inverse of a 
## square matrix.


## This function assumes a square matrix is passed 
## as an argument. The matrix object is cached and 
## the matrix inverse can also be cached in the 
## function closure when used in conjuction with
## the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Make x and inv accessable in this function closure
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## This will return the matrix
  get <- function() x
  
  ## This will reset the inv variable
  setinverse <- function(solve) inv <<- solve
  
  ## This will return inv as the matrix inverse
  getinverse <- function() inv
  
  ## Allow the functions above to be accessible
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function is used after instantiating the
## function makeCachematrix.The function returns
## the inverse of the square matrix returned by 
## makeCacheMatrix. If the inverse has has been 
## cached it will return the cached results, 
## otherwise, it will compute the inverse.

cacheSolve <- function(x, ...) {
  ## get the inverse from the makeCacheMatrix 
  ## environment (it may be null)
  m <- x$getinverse()
  
  ## If m is not null then it was cached, so we
  ## just return the cached value
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }

  ## The cached inverse matrix is not cached, so
  ## calculate it now.
  matrix <- x$get()
  m <- solve(matrix)
  
  ## Add the inverse matrix to the cache before
  ## returning the inverse matrix
  x$setinverse(m)
  
  m
}




## Below is an example showing the environment
## note how the value of m changes in the 
## environment

#>B = matrix(c(1,2,3,4),nrow = 2,ncol=2)
#>
#>
#> yy <- makeCacheMatrix(B)
#>
#>
#> ls.str(environment(yy$get))
# get : function ()  
# getinverse : function ()  
# m :  NULL
# set : function (y)  
# setinverse : function (solve)  
# x :  num [1:2, 1:2] 1 2 3 4
#>
#>
#> cacheSolve(yy)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#>
#>
#> ls.str(environment(yy$get))
# get : function ()  
# getinverse : function ()  
# m :  num [1:2, 1:2] -2 1 1.5 -0.5
# set : function (y)  
# setinverse : function (solve)  
# x :  num [1:2, 1:2] 1 2 3 4
#>
#>
#> cacheSolve(yy)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
