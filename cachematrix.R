## My functions increase efficiency by caching the inverse of a matrix
## Rather than repeatedly calculating it

## makeCacheMatrix creates a matrix object that is capable of caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<- function(y){
                x<<- y
                i<<- NULL
        }
        get<- function(){
                x
        }
        setinverse<- function(inverse){
                i<<- inverse
        }
        getinverse<- function(){
                i
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}

## cacheSolve returns the inverse of the "special" matrix
cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        } else {
                data<- x$get()
                i<- solve(data, ...)
                x$setinverse(i)
                i}
}