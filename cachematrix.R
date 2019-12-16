## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function "makeCacheMatrix" takes a square matrix as an input and return a list(say "l") with the functions "setMatrix", "getMatrix", "setInverse", "getInverse"
##using "setMatrix" you can change or set the matrix you want to do the computation on - usage:l$setMatrx(matrix you want)
##using "getMatrix" you can access the current value of the matrix - usage: l$getMatrix()
##using "setInverse" and "cachesolve" together you can set the value of inverse matrix of the given matrix - usage: l$setInvesre(Invesre matrix)
##using "getInverse" you can get the current value of the inverse matrix of a given matrix - usage: l$getInverse()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
          x <<- y
          m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## "cachesolve" is for computing the inverse of a matrix if l$getInverse() returns NULL (i.e, invesre is not calculated already)
##usage: cachesolve(list created using makeCacheMatrix)
##return a matrix which is invesre of the metrix stored in the list (use l$getMatrix to see this matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}
