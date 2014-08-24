## The functions defined below creates a matrix, cache of the inverse of the matrix in order 
## to avoid repeated solving for inverse of a matrix which is a time consuming operation

## function to create a matrix and a list of functions related to this matrix  
## 1)set()-sets the matrix in the parent matrix
## 2)get()- access the set matrix
## 3)setinverse()- sets the inverse of the matrix in parent environment
## 4)getinverse()- acess the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    	set <- function(y) {
        x <<- y
        i <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inverse) i <<- inverse
   	getinverse <- function() i
   	list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## this function tests if the inverse has been stored in the cache and returns value if it is stored. if not it computes
## the inverse and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    inverse <- solve(data)
    i<-x$setinverse(inverse)
    i
}
