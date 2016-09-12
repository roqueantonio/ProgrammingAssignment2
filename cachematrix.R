
## This function will create a matrix object capable of cacheing its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}

## This function will compute the inverse of a matrix. This matrix is 
## created using the above funciton; makeCacheMatrix(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        m
        
}
