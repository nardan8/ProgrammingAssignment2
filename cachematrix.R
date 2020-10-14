# makeCacheMatrix creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inversed
# 4.  get the value of the inversed

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
        
}


# `cacheSolve` function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix`

cacheSolve <- function(x, ...) {
        
        ## get inversed matrix from makeCacheMatrix
        invMat <- x$getInv()
        
        ## if invMat isn't NULL
        if(!is.null(invMat)){
                ## return invMat
                message("getting cached inversed matrix")
                return(invMat)
        }
        
        ## receive non inversed matrix
        data <- x$get()
        
        ## inverse received matrix and assign to invMat
        invMat <- solve(data, ...)
        
        ## save inversed matrix in cache
        x$setInv(invMat)
        
        ## Return inversed matrix
        invMat
}
