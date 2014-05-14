## Function cacheSolve is to return the inverse of a matrix either
## by calculating it or retrieving it from a cached result of the
## function makeCacheMatrix.



# makeCacheMatrix creates a special "matrix" object, i.e a list of
# 4 functions to:

# [1] set the value of the matrix in a Cache environment
# [2] get the value of the matrix
# [3] set the value of the inverse matrix in the Cache
# [4] get the value of the inverse matrix.

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



# cacheSolve returns the inverse of matrix X by calculating it or
# retrieving it from the Cache.

cacheSolve <- function(X, ...) {
        
        #Checks whether the setsolve function of makeCacheMatrix's
        #special "matrix" has already calculated the required inverse.
        
        #If yes, gets the inverse from the Cache and returns it as it
        #is without repeating the same calculation.
        
        S <- X$getsolve()
        if(!is.null(S)) {
                message("getting cached data")
                return(S)
                
        #If no, calculates the inverse, sets the value in Cache to
        #this inverse and returns the calculated value.
        
        } 
        data <- X$get()
        S <- solve(data, ...)
        X$setsolve(S)
        S
}



# Trying out how the above functions work:
TESTcachematrix <- function(X=matrix(rnorm(25),nrow=5)) {
        readline("           PRESS A KEY")
        #X <- matrix(rnorm(25),nrow=5)
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
        
        readline("           PRESS A KEY")
        M <- matrix(rnorm(9), nrow=3)
        print("inverse of a matrix with solve():")
        print(solve(M))
        
        readline("           PRESS A KEY")
        CachedM <- makeCacheMatrix(M)
        print("inverse of a matrix with 1st run of cacheSolve():")
        print(cacheSolve(CachedM))
        
        readline("           PRESS A KEY")
        print("inverse of a matrix with 2nd run of cacheSolve():")
        print(cacheSolve(CachedM))
}
