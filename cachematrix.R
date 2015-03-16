## The following two functions together calculate the inverse of 
## an invertible matrix and cache this information

## This function takes as input a matrix and returns a special matrix
## which is actually a list of the following functions:
## 1. set(): which sets the value of the matrix
## 2. get(): which gets the value of the matrix
## 3. setinverse(): which sets the value of the inverse of the matrix
## 4. getinverse(): which gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##The following function takes as input the matrix/list given by the first 
##function. First it uses the function getinverse(). If the inverse has been 
##already calculated it returns the inverse. Otherwise, it gets the matrix 
##throught the get() function, computes the inverse throught "solve"
##sets the value of the inverse in cache using the setinverse() function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
