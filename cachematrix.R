## These functions create a cache matrix, and determines if a cache of the matrix inversion 
# has already been claculated. 



## Create a list containing functions to cache and return the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## check if inverse already exists, and return cache if so. If not, cxalculate and return. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}

mat <- matrix(c(2,2,3,2), nrow = 2, ncol = 2)
mat
solve(mat)
a <- makeCacheMatrix(mat)
cacheSolve(a)


