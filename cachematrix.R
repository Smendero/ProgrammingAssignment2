## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really 
##a list containing a function to

##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(inversematrix) invm <<- inversematrix
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the mean of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse 
## matrix has already been calculated, and that the matrix in use is the same. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the 
## value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, matrix = NULL) {
        if(!is.null(matrix) & !identical(x$get(), matrix)){
                        x$set(matrix)
        }
        
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
        data <- x$get()
        invm <- solve(data)
        x$setinv(invm)
        invm
}
