## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
        ## matrix x below: is a square invertible matrix that returns a list containing functions to
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse
        ## 4. get the inverse
        ## the list is then used as an input to the cacheSolve() function.
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<-NULL                            #here i store the matrix in the cache
                
                }
        get <- function() x                                     #collect the matrix
        setInverse <- function(solve) inv <<-solve              # now set the inverse matrix
        getInverse <- function() inv                              #getting the inverse of the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                           #Creating the list of functions
}


## Write a short comment describing this function
##Calculating the Inverse of the matrix created by makeCacheMatrix function using cacheSolve
##Before calculating the inverse, cacheSolve first checks to see if the matrix has been calculated
##If calculated before, cacheSolve recalls this matrix from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		 inv <- x$getInverse()                                 #here i first request the cache of the matrix `x'
        if(!is.null(inv)){                                    #if the cache exists, then it has been calculated
                message("getting cached data")              #reporting that this is only cached data
                return(inv)                                   #returning the cache
        }
        data <- x$get()                                         #getting the matrix created by the makeCacheMatrix function
        inv <- solve(data, ...)                               #calculating the inverse of the matrix
        x$setInverse(inv)                                     #storing the inverse in a cache 
        
}
