## Dev by Steven Sotelo
## Assignment 2

## This function create a special object, It uses the cache to save a matrix.
## This pattern is similar to Singleton in other programming languages

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function return the inverse of a matrix.
## It first check the cache memory if it has a value, otherwise it calculate the inverse and save the matrix in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}

## Test the function

x_data <- matrix(c(4,3,3,2),nrow=2,ncol=2)
x_matrix <- makeCacheMatrix(x_data)
cacheSolve(x_matrix)

x_data2 <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)
x_matrix2 <- makeCacheMatrix(x_data2)
cacheSolve(x_matrix2)
