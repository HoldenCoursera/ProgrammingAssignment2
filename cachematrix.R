## The two functions below work together to prevent duplicative matrix inversion calculations.
## This is accomplished by calculating and cacheing matrix inversion calculations with the
## 'makeCacheMatrix' function, and checking to see if matrix inversion has already been calculated
## with 'cacheSolve' function. 

## This function allows for the creation of a special "matrix" object and caches the inverse
## of it.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv  <- NULL
            set  <- function(y) {
                      x <<- y
                      mat_inv <<- NULL
        }
        get_mat <- function() x 
        set_mat_inverse <- function(inverse) mat_inv <<- inverse
        get_mat_inverse <- function() mat_inv
        list(set = set, get_mat = get_mat,
             set_mat_inverse = set_mat_inverse,
             get_mat_inverse = get_mat_inverse)

}

## This function calculates the inverse of given a "matrix." It first checks to see if the inverse
## of the given "matrix" has already been calculated; if so, it returns the inverse with a message 
## denoting that is has been retrieved from memory. If not, it calculates, returns, and stores the inverse 
## of the given matrix. 

cacheSolve <- function(x, ...) {
        mat_inv <- x$get_mat_inverse()
        if (!is.null(mat_inv)) {
          message("retrieving cached matrix inverse")
          return(mat_inv)
        }
        mat <- x$get_mat()
        mat_inv <- solve(mat, ...)
        x$set_mat_inverse(mat_inv)
        mat_inv
}




