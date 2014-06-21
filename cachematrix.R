setwd('/Users/iLeo/coursera/DataScience/02-RProgramming/ProgrammingAssignment2')

## since calculating the inverse of a matrix can be an expensive operation,
## we want to provide an easy way calculate it once and keep its value cached


## this function holds internally a matrix and its cached inverse and provides some useful function to get/set the matrix we are going to invert and to get/set its inverse

makeCacheMatrix <- function(m = matrix()) {

  ## the matrix's inverse we are caching
  cachedInvertedMatrix <- NULL
  
  ## if we change the matrix we have to set 
  set <- function(newMatrix) {
    m <<- newMatrix
    cachedInvertedMatrix <<- NULL
  }
  
  ## function to get the matrix we are working on
  get <- function() m
  
  ## function to set its inverse
  setSolve <- function(invertedMatrix) cachedInvertedMatrix <<- invertedMatrix
  
  ## function to get its inverse
  getSolve <- function() cachedInvertedMatrix
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## this function calculates the inverse of the matrix contained in the cacheMatrix parameter.
## if there's a valid cache it just returns it, 
## else it calculates the correct value, set it as cached in the cacheMatrix param
## and returns it
cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheMatrix'
  
  ## get the current cached inverted matrix 
  invertedMatrix <- cacheMatrix$getSolve()
  
  ## is it valid? 
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    
    ## bingo, return it!
    return(invertedMatrix)
  }
  
  ## read the actual matrix stored in our object
  data <- cacheMatrix$get()
  
  ## calculate its inverse
  newInvertedMatrix <- solve(data, ...)
  
  ## set it as cached value in the cacheMatrix object
  ## (so at the next call we'll return it from the cache instead of recalculating it again)
  cacheMatrix$setSolve(newInvertedMatrix)
  
  ## return the just calculated inverse
  newInvertedMatrix
}

## example:
## create a new 2x2 matrix
testMatrix <- matrix(c(1,2,3,4), 2, 2)

## create a cachedMatrix object, passing the test matrix as param
cachedMatrix <- makeCacheMatrix(testMatrix)

## the 'getSolve' still returns null, because we didn't calculate it yet
print(cachedMatrix$getSolve())

## invert it & print the value
print(cacheSolve(cachedMatrix))

## now the 'getSolve' returns the correct value
print(cachedMatrix$getSolve())

## if we change the matrix...
newMatrix <- matrix(c(5,6,7,8), 2, 2)
cachedMatrix$set(newMatrix)

## ...the cached inverse will be automatically set to NULL
print(cachedMatrix$getSolve())

## until the next call to cacheSolve
print(cacheSolve(cachedMatrix))


