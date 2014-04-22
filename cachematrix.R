## makeCacheMatrix and cacheSolve using concept of caching daa
## to save time in Matrix inversion computation. 
## caching data is to store data in memory for future use. 

## makeCacheMatrix function caches the inverse of a matrix with
## the help of subfunctions set(sets the given matrix), 
## get(returns the given matrix), setinverse(sets the inverse of
## given matrix), getinverse(returns the inverse of given matrix).

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse<-function(solve) i <<-solve
	getinverse<-function()i
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve checks if there inverse of a matrix is cached in memory. 
## If there is an inverse of matrix cached then it prints the message 
## "getting cached data" and retrieves and prints the inverse of matrix.
## Else it calls for the function 'solve' to calculate the inverse of the
## matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<-x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data<-x$get()
	i<-solve(data, ...)
	x$setinverse(i)
	i

}
