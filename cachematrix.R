
## autor: Héctor Soto
## country: Chile,
## date: 1/07/2019

## Assignment: Caching the Inverse of a Matrix / Almacenar en Caché la Inversa de una matriz

## I will following the example "Caching the Mean of a Vector" given on the assignments

## In this Assignment, I will use the <<- operator to assign a value to 
##  an object in an environment that is different from the current environment. 

## In this Assigment, the operator was use for create a matrix object type  
## that stores a matrix vector and cache's its the matrix inverse.

## first function ##

## makeCacheMatrix, it is a function to create a special "matrix" object, With this function we can do:

## set the value of the vector/Matrix
## get the value of the vector/Matrix
## set the value of the Inverse Matrix
## get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  ## define Cache variable
  m <- NULL
  
  ## Assign the input matrix "y" to the variable "x" in the parent environment
  ## And re set variable "m" to "NULL" again, now en the parent environment. 
  
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  
  get <- function() x
  
  ## set caché of "m" equal to the inverse of "x"
  setinverse <- function(inverse) m <<- inverse  
  
  ## return cached matrix of x
  getinverse <- function() m 
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The following function calculates the "inverse" of the "vector/Matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the inverse of Matrix 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If value is different to NULL or the value returned is TRUE, then get the cache value of "x" 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## for to use functions created, is necessary to nested both. -->  cacheSolve(makeCacheMatrix(x))
