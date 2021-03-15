## When a matrix is entered as an argument, this code will check to see if the 
## inverse is already in memory. If it is, it will return the cached value.
## If it is not in memory, it will solve for the inverse.

## This function sets up a list of functions to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) { ##makes a function with argument x, which is a matrix
        inver <- NULL ##creates inver and sets the value to NULL
        set <- function(y) { ##makes a function for the user to reset x
                x <<- y     ##changes the value of x in the parent environment to y, the argument of the set function
                inver <<- NULL  ##clears the cache of inver
        }
        get <- function() x ##creates function get with no argument that prints x
        setinver <- function(inv_mat) inver <<- inv_mat ##creates function with 
        getinver <- function() inver #creates function with no arguments that prints inver
        list(set = set, get = get, ##creates a list of the functions so they can be called by cacheSolve
             setinver = setinver,
             getinver = getinver)
}


## Checks to see if the inverse of the x matrix has already been solved.
## If it has, it returns the value from memory. Otherwise, it solves the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver() ##gets the value of inver from MakeCacheMatrix
        if(!is.null(inver)) { ##checks to see if the value is not NULL
                message("getting cached data")
                return(inver) ## returns inverse if inver not NULL
        }
        data <- x$get() ##brings in x matrix from get function
        inver <- solve(data, ...) ##solves for the inverse of the matrix
        x$setinver(inver) ## uses setinver function to store inverse in memory
        inver        ##returns inverse
}

        
       
