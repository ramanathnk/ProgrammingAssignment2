## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  set.inverse.matrix <- function(m) inverse.matrix <<- m
  get.inverse.matrix <- function() inverse.matrix
  list(set = set, get = get,
       set.inverse.matrix = set.inverse.matrix,
       get.inverse.matrix = get.inverse.matrix)
}


## Write a short comment describing this function


cache.inverse <- function(x, ...) {
  m <- x$get.inverse.matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$set.inverse.matrix(m)
  m
}

inverse <- function(data, ...) {
  solve(data)
}


list <- makeCacheMatrix(matrix(sample(25), nrow = 5))
cache.inverse(list)
print(list$get())
print(list$get.inverse.matrix())
print(list$get.inverse.matrix() %*% list$get())
print(list)

