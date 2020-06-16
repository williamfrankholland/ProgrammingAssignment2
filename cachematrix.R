## Functions for the R Programming course Assignment 2
## makeCacheMatrix 
## CacheSolve

## makeCacheMatrix creates a list of functions that allow a matrix and it inverse
## to be stored and retieved from a cache

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix to NULL
  inverseMat <- NULL
  
  ## SetMat stores the new matrix if different from the current
  setMat <- function(mat) {
    if ( !identical(mat, x))
    {
      ## Store new matrix mat and reset the inverse to NULL
      x <<- mat
      inverseMat <<- NULL
    }
  }
  ## Function to retun cached matrix
  getMat <- function() x
  
  ## Create functions to set and get the inverse
  setInv <- function(imat) { inverseMat <<- imat }
  getInv <- function() { inverseMat }
  
  ## Return a list of the functions to set and get the cache for the matrix
  list( setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv )
}


## Return a matrix that is the inverse of 'x'
## If it is the same matrix currently stored use the cached version of the inverse

cacheSolve <- function(x, ...) {
  ## If the cache object is NULL return NULL
  if (is.null(x)) {
    NULL
  } else
  ## if the inverse matrix in cache is null and we have a non null matrix
  if ( is.null( x$getInv()) && !is.null(x$getMat()) ) {
    ## Solve for the inverse matrix
    print("Store in cache")
    x$setInv(solve(x$getMat()))
  } 
  else {
    print("Using cache")
    x$getInv()
  }
}

## Code to test out the functions above

## Create the cache and functions to access the cache with no values
my_cache <- makeCacheMatrix(NULL)

## create 2 matricies to test the cache function
my_mat1 <- matrix( c(1,2,2,1), nrow=2, ncol=2)
my_mat2 <- matrix( c(1,3,3,1), nrow=2, ncol=2)

## Calculate the inverse of the first matrix
my_cache$setMat(my_mat1)
cacheSolve(my_cache)

## Test the cache works by calling with the same matrix
cacheSolve(my_cache)

## Call the cacheSolve function with diffent values to test it out
my_cache$setMat(my_mat2)
cacheSolve(my_cache)

