## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        SetInverse <-function(solve) {
                if (det(x)!= 0) {
                        solve(x)
                } 
                else {
                        message("no inverse")
                        return(x)
                }
                
                m <<- solve} 
        GetInverse <- function() m
        list(set=set,get=get,
             SetInverse = SetInverse,
             GetInverse = GetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m = x$GetInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data = x$get()
        if(det(data) !=0){
        m = solve(data, ...)
        x$SetInverse(m)
        m}
        else{
                message("no inverse")
                
        }
}
