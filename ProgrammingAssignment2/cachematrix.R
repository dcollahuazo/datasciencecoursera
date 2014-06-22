##The function will first make sure that inv is assigned a null value
##for the matrix setting. Then it will create the get function which
##displays the created matrix. After two other functions, one  which makes
##the inversion of the matrix and the other which will display the inversion.
##Finally a list which makes sure that all variables are set in order to
##be able to access them from cacheSolve().

## Contrary to the function in the example, this function in order
##to create a matrix has to receive 3 parameters. It could be done like this or 
##the function should only receive a sqared invertible matrix.

makeCacheMatrix <- function(x = matrix(), rows = numeric(), cols = numeric()) {
        inv <- NULL
        set <- function(y, r, c) {
                x <<- matrix (y, r, c)
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


##This function performs the inversion of the matrix and saves it into 
##memory (as a variable). However, the function first checks whether the 
##inversion has already been achieved before or not. In the case, the 
##inversion was achieved, an alert will be returned accompained with the 
##already in memory inversion result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
