## These two functions serve to define a matrix and calculate and store its inverse. 

## This function creates a vector to store matrix and inverse-matrix data and function calls to
## save, change, and display matrix and inverse-matrix data

makeCacheMatrix <- function(x = numeric(), nrows, ncols) {
        #create a matrix from the function variables        
        m <- matrix(x, nrows, ncols)
        
        #create an empty variable to store the inverted matrix later
        i <- NULL
        
        #define a function that allows to change the content of the CacheMatrix after the vector has already been defined without re-writing the whole thing
        set <- function(y, nrows2, ncols2) {
                #Check whether the exact same CacheMatrix is already defined and if so...
                if(identical(matrix(y, nrows2, ncols2), m)) {
                        
                        #...stop the execution of the function so that i is not reset and does not need to be recalculated
                        stop("Exact same matrix already defined")
                }
                #if the same matrix is not already identified, save the new CacheMatrix and clear i
                else {
                        m <<- matrix(y, nrows2, ncols2)
                        i <<- NULL
                }
        }
        #define a function that returns the value of the ChacheMatrix
        get <- function() m
        
        #define a function that allows to store the calculated inverse Matrix
        setinverse <- function(invert) i <<- invert
        
        #define a function that returns the calculated inverse Matrix
        getinverse <- function() i
        
        #make a list including all the functions defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function inverts the matrix defined above and checks whether a pre-stored version of the
## inveted matrix already exists. If so, the inverted matrix is retreived from the ChacheMatrix vector
## instead of recalculating it

cacheSolve <- function(m, ...) {
        #load the i variable
        i <- m$getinverse()
        
        #check whether the i variable is not empty. if not, get the value that is already stored for it and - instead of reculculating it - print the "cached" version of i with a note saying "getting cached data"
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #if i is empty: get the matrix data stored in variable "m"
        data <- m$get()
        
        #...invert it and store it in the variable i...
        i <- solve(data, ...)
        
        #...and execute the "setinverse" function that stores i in the CacheMatrix vector
        m$setinverse(i)
        
        #lastly, print i
        i
}