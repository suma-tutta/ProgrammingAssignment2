## The purpose of the following 2 functions is to cache the inverse of a matrix  
## and reuse when necessary without actually re-calculating the matrix inverse
## each time it is used 

## Function makeCacheMatrix can be used to define a matrix and subsequently 
## access the matrix and its inverse (if already calculated)

## Function cacheSolve performs one of the following
##    Retrieves the inverse of a matrix (if already calculated and cached)
##    If not already calculated, calculates the inverse of a matrix


## makeCacheMatrix: Create a special matrix (square) that has the following properties
## 1. Sets the values of the matrix
## 2. Gets/displays the matrix
## 3. Sets the Inverse of the matrix
## 4. Gets/displays the inverse of the matrix

makeCacheMatrix <- function(x = matrix(nrow=0, ncol=0)) {

        mInv <- NULL
        set <- function(y) {
            x <<- y
            mInv <<- NULL
        }
        get <- function() x
        setInv <- function(InverseMatrix) mInv <<- InverseMatrix
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## The function cacheSolve takes a square matrix as an argument and returns
## the inverse of the square matrix (created using function makeCacheMatrix)
## The function while returning the matrix inverse, checks 
## If the matrix inverse is already calculated, 
##      If yes,the cached matrix inverse is returned.
##      Otherwise matrix inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        mInv <- x$getInv()
        if(!is.null(mInv)) {
            message("getting cached data")
            return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv
    
}
