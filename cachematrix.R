## Put comments here that give an overall description of what your
## functions do

## Matrix Cache Management

makeCacheMatrix <- function(x = matrix()) {
       # initializes cached matrix as empty (NULL)
       inv_matrix <- NULL
       set <- function(y) {
              x <<- y
              inv_matrix <<- NULL
       }
       get <- function() x
       setInverse <- function(inverse) inv_matrix <<- inverse
       getInverse <- function() inv_matrix
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## Matrix Inverse by solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv_matrix <- x$getInverse()
       # checks if the inversed matrix already has values
       if(!is.null(inv_matrix)) {
              # there are no values, so it refreshes the data
              message("getting cached data...")
              return(inv_matrix)
       }
       
       data <- x$get()
       inv_matrix <- solve(data, ...)
       x$setInverse(inv_matrix)
       inv_matrix
}
