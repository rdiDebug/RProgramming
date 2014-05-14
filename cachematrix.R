## Put comments here that give an overall description of what your


# makeCacheMatrix creates a special "matrix" object, i.e a list of
# 4 functions to:

#[1] set the value of the matrix in a Cache environment
#[2] get the value of the matrix
#[3] set the value of the inverse matrix in the Cache
#[4] get the value of the inverse matrix.

makeCacheMatrix <- function(X = matrix()) {
        S <- NULL
        set <- function(Y) {                    #1st function-to-be
                X <<- Y
                S <<- NULL
        }
        get <- function() X                     #2nd function-to-be
        setsolve <- function(solve) S <<- solve #3rd function-to-be
        getsolve <- function() S                #4th function-to-be
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)               #in the list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


#Trying out how the above functions work
TESTcachematrix <- function() {
        readline("           PRESS A KEY")
        X <- matrix(rnorm(25),nrow=5)
        print("This is the original matrix:")
        print(X)
        
        readline("           PRESS A KEY")
        CachedX <- makeCacheMatrix(X)
        print("This is the cached matrix:")
        print(CachedX$get())
        
        readline("           PRESS A KEY")
        print("This must be cached as inverse before any calc:")
        print(NULL)
        
        readline("           PRESS A KEY")
        print("This is really cached as inverse before any calc:")
        print(CachedX$getsolve())
        
}
