## This pair of functions "makeCacheMatrix" and "cacheSolve" are used to create
## a special matrix and cache the inverse of the matrix.


## makeCacheMatrix takes on a matrix, builds four functions( set(),get(),
## setinverse(), getinverse() ), and returns a list of functions to the parent 
## environment.


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL       ## initialize x and inver that store key information 
        set <- function(y) {
                x <<- y
                inver <<- NULL}
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse        
        getinverse <- function() inver       
        ## R retrieve inver value in the parent environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## creates a list of function and returns it
}



## cacheSolve returns the inverse of the matrix created in makeCacheMatrix.
## It first check whether the inver is NULL, if is TRUE, it means we have a 
## cached inverse and will print "getting cached data" and return it to the
## parent environment. If the condition is FALSE, R will solve the inverse and
## set the inverse in the inver and then returns the value of the inver to the
## parent environment.


cacheSolve <- function(x, ...) {
        
        inver<-x$getinverse()
        
        if(!is.null(inver)){
                message("getting cached data")  ## if the condition is TRUE
                return(inver)}
        
        data<- x$get()  
        inver<-solve(data,...)
        x$setinverse(inver)
        inver
        ## if the condition is FALSE
}
