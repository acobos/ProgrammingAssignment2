## The following functions can be used to create an object (first function)
## that stores a matrix and caches its inverse (second function).

## This function creates an object (list) containing four functions:
## 1. $set: a setter to create a matrix
## 2. $get: a getter to get the matrix
## 3. $set.inverse: a setter for the inverse of the matrix
## 4. $get.inverse: a getter for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
                x <<- y
                i <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) i <<- inverse
    get.inverse <- function() i
    # returning the list of functions
    list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## This function verifies if the inverse has been previously calculated;
## if so, returns the stored inverse; 
## otherwise, computes and returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get.inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set.inverse(i)
    i
}

# validation
# m = makeCacheMatrix()
# m$set(matrix(1:4,2,2))
# m$get()
# m$get.inverse()
# cacheSolve(m)
# m$get.inverse()
# m$get() %*% m$get.inverse()
