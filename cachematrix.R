## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m<- NULL
	 set<- function(y) {
	        x <<- y
            m <<- NULL
			}
			get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse) ##result is a list containing a function to
				                          ##1.  set the value of the vector
                                          ##2.  get the value of the vector
                                          ##3.  set the value of the mean
                                          ##4.  get the value of the mean
}


## it first checks to see if the inversen has already been calculated. If so, it `get`s the `cacheSolve` retrieve the inverse from the
##cache and skips the computation. Otherwise, it inverse of
##the data and return it.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
		if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
		data <- x$inverse()
            m <- inverse(data, ...)
            x$setinverse(m)
            m
        ## Return a matrix that is the inverse of 'x'
}
