## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                print("#####Getting cached data")
                return(mInverse)
        }
        data <- x$get()
        print("#####Solving inverse....")
        mInverse <- solve(a=data) #b is missing argument, hence will be taken as identity
        x$setInverse(mInverse)
        mInverse
}

test <- function()
{
        m1 <- matrix(1:4, nrow=2, ncol=2)
        m2 <- matrix(c(3,0,0,0,3,0,0,0,3), nrow=3, ncol=3)

        print(m1)
        print(solve(a=m1))
        
        cm1 <- makeCacheMatrix(m1)
        cm1$set(m1)
        inv <- cacheSolve(cm1)
        print(inv)
        #print(m1 %*% m1inverse)
        #print(m1inverse %*% m1)
        
        print(m2)
        print(solve(a=m2))
        cm2 <- makeCacheMatrix(m2)
        cm2$set(m2)
        inv <- cacheSolve(cm2)
        print(inv)
        #print(m2 %*% m2inverse)
        #print(m2inverse %*% m2)
        
        inv <- cacheSolve(cm1)
        print(inv)
        
        inv <- cacheSolve(cm2)
        print(inv)
}
