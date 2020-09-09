## Put comments here that give an overall description of what your
## functions do

## this function take an object x, that it must to be a matrix, and create a list
## which contains 4 functions that set and get the values of matrix x and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set.inv <- function(i) inv <<- i
        get.inv <- function() inv
        list(set = set, get = get , 
            set.inv = set.inv, 
            get.inv = get.inv)
}


## This function take the list created by makeCacheMatrix function,
## computes the inverse of the matrix x and cache it

cacheSolve <- function(x, ...) {
        inv <- x$get.inv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$set.inv(inv)
        inv
}
