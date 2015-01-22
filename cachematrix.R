# 
#    cachematrix.R
#
#    R code for computing the inverse of a matrix with 
#    caching. This is useful when the inverse of a 
#    matrix needs to be accessed multiple times, say, 
#    in a loop.
#
#    The below code makes use of the example code (accessed 22.1.2015)
#
#       https://github.com/rdpeng/ProgrammingAssignment2
#
#    for computing the mean of a vector with caching.
# 


#
#  makeCacheMatrix(x)  --- Create object to store cached matrices.
#
#  For usage, see the 'cacheSolve' function, or testing section below. 
#
makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverse <- NULL
    
    # update the matrix 'x' (which we might want to invert)
    set <- function(y) {
        # Note: 'x' is already a variable in the parent environment; it is
        # the variable passed to 'makeCacheMatrix'. The <<- operation
        # sets the value of this 'x' into 'y'.
        x <<- y
        
        # When updating the matrix, we invalidate any cached inverse matrix.
        cachedInverse <<- NULL
    }
    
    # return the matrix 'x'
    get <- function() x
    
    # used by cacheSolve to store the inverse of the matrix 'x'.
    setinverse <- function(newInverse) {
        cachedInverse <<- newInverse
    }
    
    # return the cached inverse of 'x'
    getinverse <- function() {
        cachedInverse
    }
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#
#  cacheSolve(x, ...) -- returns the inverse of a matrix created using 
#                        'makeCacheMatrix'.
#
#  Parameters:
#     x    the input square matrix. It is assumed to be invertible. 
#     ...  optional parameters passed to solve. 
#
#  Example use:
#     # create 7x7 identity matrix
#     id7x7 <- diag(7)
#     # create makeCacheMatrix from id7x7
#     cm7x7 <- makeCacheMatrix(id7x7)
#     # Solve the inverse (which is again the 7x7 identity matrix)
#     cacheSolve(cm7x7)
#
cacheSolve <- function(x, ...) {
    # Have we already computed the inverse?
    m <- x$getinverse()
    if(!is.null(m)) {
        # Yes: return the cached inverse matrix.
        message("Returning the cached inverse matrix...")
        return(m)
    }
    # No: Compute, cache and return the inverse matrix.
    m <- solve(x$get(), ...)  # solve the inverse
    x$setinverse(m)           # cache it
    m                         # return inverse
}


# ------------------------- Testing -----------------------
#
#   The matrix inverse satisfies:
#      matrix * (inverse(matrix)) = identity matrix
#   and   
#      inverse(matrix) * matrix = identity matrix
#
#   Below is commented code for checking that this is true 
#   (up to the precision of floating point arithmetics.)
#   It is worth observing that in this code, 'cacheSolve' 
#   is called twice, but the inverse is only computed once. 
#
#    ###  return (dim x dim) matrix with random entries. 
#    randomMatrix <- function(dim) {
#        matrix(data=runif(dim*dim), nrow=dim, ncol=dim)
#    }
#
#    dim <- 700
#    cm <- makeCacheMatrix(randomMatrix(dim))
#    ### (Note: here %*% is matrix multiplication)
#    norm(cm$get() %*% cacheSolve(cm) - diag(dim))
#    norm(cacheSolve(cm) %*% cm$get() - diag(dim))
#
#