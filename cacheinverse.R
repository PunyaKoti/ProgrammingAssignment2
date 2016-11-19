## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix creates an R object that stores the Matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        M<-matrix(data=NA,nrow(x),ncol(x)) #initialising an empty matrix with NA
        
        set <- function(y) {
                x <<- y
                M<-matrix(data=NA,nrow(x),ncol(x)) #initialising an empty matrix with NA
        }
        get <- function() x
        setinverse <- function(inverse) M <<- inverse
        getinverse <- function() M
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}        

## CacheInverse calculates the inverse of the matrix created with the above function 'makeCacheMatrix'. 
## If the inverse has already been calculated, it retrieves the inverse from the cache, otherwise it 
## calculates the inverse and stores in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        if(!all(is.na(M))) { #checking whether inverse has already been computed
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)  #inverse calculation 
        x$setinverse(M)
        M
}