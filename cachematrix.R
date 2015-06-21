## This programming will allow calculate a mean of vector, in a fast operation 
## without taking to much time a resources of your computer

## makeCacheMatrix is a function that creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(inverse) m <<- inverse
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}



## The next function returns the inverse of the matrix. First checks to see 
## if the mean has already been calculated. If so, it gets the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of the 
## data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
       m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m
}

