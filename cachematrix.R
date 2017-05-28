## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCAcheMatrix makes functions above:
## set the matrix value; get the value of the matrix; get the value of inverse of the matrix; get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## this function creates an inverse of the matrix created above. 
##If the inverse is already calculated this function retrive it from a cache. 
##If not it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}


##TESTING
##> TMatrix <- makeCacheMatrix (matrix(1:4,2,2))
##> TMatrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> TMatrix$getInverse()
##NULL
##> cacheSolve(TMatrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(TMatrix)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
