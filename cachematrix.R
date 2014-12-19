## These functions, combined, are able to compute an inverted matrix
## and keep it cached. The cached version is retrieved when possible,
## sparing computational resources and improving performance.

## Creates a list containing functions, the original matrix,
## and the inverted one.

makeCacheMatrix <- function(x = matrix()) {
    inv.matrix <- NULL
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }
    get <- function() x
    setInverted <- function(inv.matrix) inv.matrix <<- inv.matrix
    getInverted <- function() inv.matrix
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## Returns the inverse of the matrix included in the makeCacheMatrix "x"
## argument. Computes the inverse only if not already cached by "x".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv.matrix <- x$getInverted()
    if(!is.null(inv.matrix)) {
        writeLines("\nGetting cached matrix...\n")
        return(inv.matrix)
    }
    ori.matrix <- x$get()
    inv.matrix <- solve(ori.matrix, ...)
    x$setInverted(inv.matrix)
    inv.matrix
}
