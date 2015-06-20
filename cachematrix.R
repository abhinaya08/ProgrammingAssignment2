## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initializing the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y=matrix()) {
                x <<- y
                inv <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() {
                ##Returning the matrix
                x
        }
        
        ## Method to set the inverse of the matrix
        setinv <- function(inv_matrix){ 
                ##Storing the matrix
                inv <<- inv_matrix
        }
        
        ## Method to get the inverse of the matrix
        getinv <- function() {
                ##returns the inverse
                inv
        }
        
        ##Returns the list of methods
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        ##Checking if inverse has already been calculated to be returned
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##Following runs if the inverse hasnt already been calculated
        
        ##Gets the martix
        data <- x$get()
        
        ##Calculates the inverse using solve() function
        inv <- solve(data)
        
        ##Saves the inverse in the cache
        x$setinv(inv)
        
        ##Returns the inverse
        inv
}
