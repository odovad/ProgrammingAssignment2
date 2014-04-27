## Put comments here that give an overall description of what your
## functions do

## Create the object that contains functions to set/get the matrix and the inverse 

makeCacheMatrix <- function(x = matrix()) {
	#Initialize the inverse 
	inverseMatrix <- NULL
	
        # Set 
	set <- function(new){
	    x <<- new
	    inverseMatrix <<- NULL
	 }
	 
        # Get
	 get <- function() x; 

	 setInverse <- function(inverse){
	 	    inverseMatrix <<- inverse
         }
	 getInverse <- function() inverseMatrix
        
        # Define the list that contains all functions defined earlier
	 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## Calculate the inverse if it's not defined.
## If the inverse already exists, returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
        # If the inverse is cached ...
	if (!is.null(m)) {
	   message("getting cached inverse")
	   return(m)
	}
        # Calculate the inverse
	data <- x$get();
	inverseMatrix <- solve(data)
	x$setInverse(inverseMatrix)
	inverseMatrix
}
