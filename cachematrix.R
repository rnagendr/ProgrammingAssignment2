## The short comment describing the function
# makeCacheMatrix function takes argument as matrix whose inverse is to be found
# The inverse of the function calculated using cacheSolve function is cached
# makeCacheMatrix function returns  a list of functions to set and get cached inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}
## The short comment describing the function
# cacheSolve function checks for cached inverse function and if it exists, the function returns the same 
# if cached inverse matrix is not available, it calculates the inverse and returns the same
cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
# To test the output
a<-c(1,-3,5,-2,1,4,6,-2,1)
b<-matrix(a,3,3)
cacheSolve(makeCacheMatrix(b))
