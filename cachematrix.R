## Put comments here that give an overall description of what your
## functions do

## This function receives a matrix as input and outputs a list of 4 functions:
## a function to set the value of the matrix in a variable which is outside the 
## working environment
## a function to retrieve the stored matrix
## a function to store (cache) the inverse of the matrix in a variable outside the
## working environment.
## finally a function to retrieve the value of the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## This function receives as input a list created by the makeCacheMatrix function
## above. It first checks whether the inverse of the matrix that was used as 
## argument in the above function has already been stored in the cache and if so
## it retrieves it and prints it out, after the message "getting cached data".
## If not, it performs matrix inversion to the argument matrix, stores that to
## the cache for future retrieval and outputs the ivnerse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
