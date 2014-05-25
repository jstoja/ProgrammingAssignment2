## Set of two functions to cache the matrix inversion operation.
## The methods made available through the wrapped matrix are:
## get() => return the original matrix data
## set(matrix) => set a new matrix data in the wrapped matrix
## getInverse() => return the last set inverse matrix
## setInverse(inverse_matrix) => set a new inverse matrix according to the matrix data

## Function creating the CacheMatrix wrapper by returning a list of functions as described above.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  get <- function() x
  set <- function(new_x) {
    x <<- new_x
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  setInverse <- function(new_inverse) inverse <<- new_inverse
  
  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setInverse = setInverse
  )
}

## Function returning the inverse matrix using the cached value if already calculated.
## This is a memoized version of the solve function applied to the CacheMatrix object type.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (is.null(inverse)) {
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
  }
  inverse
}
