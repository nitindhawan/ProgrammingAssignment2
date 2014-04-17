## Put comments here that give an overall description of what your
## functions do
## 
## Steps:: 
## cMatrix <- makeCacheMatrix()         # constructs a cache matrix inside, returns a list of functions
## cMatrix$set(mat)                     # set value of your matrix
## cacheSolve(cMatrix)                  # first time it will compute and cache the matrix inverse
## cacheSolve(cMatrix)                  # it will return matrix inverse from cache


## makeCacheMatrix returns you list of functions for get, set, setInverse and getInverse
## Common usage :
## cMatrix <- makeCacheMatrix()         # constructs a cache matrix inside, returns a list of functions
## cMatrix$set(mat)                     # set value of your matrix
## cacheSolve(cMatrix)                  # first time it will compute and cache the matrix inverse
## cacheSolve(cMatrix)                  # it will return matrix inverse from cache
makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(mat) mInverse <<- mat
        getInverse <- function() mInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve checks if the matrix's inverse is cached, then it returns the cached inverse
## Else it computes and stores the inverse and also return the inverse
## if the matrix is non-invertible, cacheSolve will crash with the same error as solve
## Common usage :
## cMatrix <- makeCacheMatrix()         # constructs a cache matrix inside, returns a list of functions
## cMatrix$set(mat)                     # set value of your matrix
## cacheSolve(cMatrix)                  # first time it will compute and cache the matrix inverse
## cacheSolve(cMatrix)                  # it will return matrix inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                message("#####Getting cached data")
                return(mInverse)
        }
        data <- x$get()
        message("#####Solving inverse....")
        mInverse <- solve(a=data) #b is missing argument, hence will be taken as identity
        x$setInverse(mInverse)
        mInverse
}

test <- function()
{
        m1 <- matrix(1:4, nrow=2, ncol=2)
        m2 <- matrix(c(3,0,0,0,3,0,0,0,3), nrow=3, ncol=3)
        m3 <- matrix(1:9, nrow=3, ncol=3)

        print(m1)
        print(solve(a=m1))
        
        cm1 <- makeCacheMatrix()
        cm1$set(m1)
        inv <- cacheSolve(cm1)
        print(inv)
        #print(m1 %*% m1inverse)
        #print(m1inverse %*% m1)
        
        print(m2)
        print(solve(a=m2))
        cm2 <- makeCacheMatrix()
        cm2$set(m2)
        inv <- cacheSolve(cm2)
        print(inv)
        #print(m2 %*% m2inverse)
        #print(m2inverse %*% m2)
        
        inv <- cacheSolve(cm1)
        print(inv)
        
        inv <- cacheSolve(cm2)
        print(inv)
        
        #cm3 <- makeCacheMatrix()
        #cm3$set(m3)
        #inv <- cacheSolve(cm3)
        #print(inv)
}
