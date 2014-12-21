## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

# Matrix inversion is usually a costly computation and their may be 
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly. This assignment contains a pair of functions that cache 
# the inverse of a matrix.
# 
# Following functions are used for caching the inverse of a matrix:
#   
# 1. makeCacheMatrix: This function creates a special "matrix" object 
#    that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been calculated
#    (and the matrix has not changed), then the cacheSolve  retrieve the 
#    inverse matrix from the cache.
#    Computing the inverse of a square matrix can be done with the solve function in R. 
#    For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, the assumption is that the matrix supplied is 
# always invertible.
# 


#_________________________________________________________
#*********************************************************
## Description of makeCacheMatrix function
#*********************************************************

# The makeCacheMatrix function creates a a list containing functions to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse of the matrix
#   4. get the value of the inverse of the matrix
#_________________________________________________________

makeCacheMatrix <- function(x = matrix()) {
  # x equal to empty matrix, set inverse matrix 'i' to null
  i <- NULL
  
  set <- function(y){
    x <<- y             #set function assigns matrix 'y' to 'x'
    i <<- NULL          #inverse matrix 'i' set to null
  }
  
  
  get <- function() x   #get function returns matrix 'x'
  
  # setInverse calculates inverse matrix and
  # overrides the previous inverse matrix 'i' 
  setInverse <- function(solve) i <<- solve
  
  
  getInverse <- function() i #returns inverse matrix 'i'
  
  # creates list of the functions 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


#_________________________________________________________
#*********************************************************
## Description of cacheSolve function
#*********************************************************
#   The cacheSolve function calculates the inverse of the special "matrix"
#   created with the makeCacheMatrix function above.

#   1. First check to see if the inverse has already been calculated.
#   2. If so, get the inverse from the cache and skips the computation.
#   3. Otherwise, calculate the inverse of the matrix 
#      and set the value of the inverse using the setinverse function
#_________________________________________________________



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  i <- x$getInverse()     # Retrives the most recent inverse matrix
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)             # Returns cached inverse matrix
  }
  
  # If the value of Inverse matrix is NULL 
  data <- x$get()         #Retrive matrix 'x' 
  message("displaying calculated inverse matrix")
  i <- solve(data, ...)   #Calculate inverse matrix of 'x'
  x$setInverse(i)         #Set inverse matrix of 'x' 
  i                       #Return inverse matrix
  
}
