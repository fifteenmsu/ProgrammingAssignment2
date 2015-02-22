##   Two functions are implemented here:
##      1) makeCacheMatrix(x)
##             create a special object from the input 
##             matrix 'x' to store its Inverse.
##
##      2) cacheSolve(x, ...)
##             calculates inverse of input matrix 'x' created with 
##              makeCacheMatrix function. If 'x' has been already inversed - just return cached inverse.



## makeCacheMatrix returns a list of function to set & get _value_ of matrix
## and also to set & get _inverse of matrix_ - setInvMtrx, getInvMtrx.

makeCacheMatrix <- function(x = matrix()) {
        InvMtrx <- NULL
        set <- function(y) {
                x <<- y
                InvMtrx <<- NULL
        }
        get <- function() x
        setInvMtrx <- function(IM) InvMtrx <<- IM
        getInvMtrx <- function() InvMtrx
        list(set = set, get = get,
             setInvMtrx = setInvMtrx,
             getInvMtrx = getInvMtrx)
} 


## cacheSolve returns inverse of input matrix: call solve(...) to inverse for
## the first time, then if inverse already exists - just returns it without
## recalculating 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InvMtrx <- x$getInvMtrx()
        if(!is.null(InvMtrx)) {
          message("getting cached inverse of matrix")
          return(InvMtrx)
        }
        Mtrx <- x$get()
        InvMtrx <- solve(Mtrx, ...)
        x$setInvMtrx(InvMtrx)
        InvMtrx
}