# coursera R Programming assignment2
# author: Jianqiu Wang
# email: jw2329 [at] cornell [dot] edu
# date: April 15, 2020

# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL # reset cached value to NULL
    set <- function(y)     
    {
        x <<- y
        inverse_matrix <<- NULL # reset value of inverse matrix in parent env
    }
    get <- function() x
    setinverse <- function(solve) inverse_matrix <<- solve
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if (!is.null(inverse_matrix))
    {
        message("getting cached data")
        return(inverse_matrix) # return to parent env
    }
    data <- x$get()
    inverse_matrix <- solve(data)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
