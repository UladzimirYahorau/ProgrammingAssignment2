## The first function has a square matrix as its argument. 
## It returns the list of 4 functions which can be used to 
## change the value of this matrix (set_matrix), get the value (get_matrix()),
## get and/or set the inverse of this matrix (get_inverse, set_inverse). 

## Notice also that, for example, we use <<- operator in set_matrix function since we call
## this function from the different environment and we want to make sure that the value
## of, say, x changes after we call this function. Also notice, that we set inverse to be
## NULL when we change the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        changed <- TRUE
        inverse <- NULL
        
        set_matrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        get_matrix <- function() x
        
        set_inverse <- function(z){
                inverse <<- z 
        }
        
        get_inverse <- function() inverse
        
        list(set = set_matrix, get = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
        
}


## The following function take as an argument the object created by the previous 
## function. It returns the inverse of the matrix associated with the matrix object.  
## The inverse is taken from the cache if this value have already been computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        matrix <- x$get()
        inverse <- solve(matrix)
        x$set_inverse(inverse)
        inverse
}
