## R-programming: Assignment 2
## INVERTING A SQUARE MATRIX APPLYING THE SOLVE FUNCTION

## The first function 
## 1: set the matrix, 
## 2: get the matrix, 
## 3: set the inverse matrix and 
## 4: get the inverse matrix

## The second function calculates the inverse matrix of 
## the special "matrix" created with the first function.

## Example matrix:
Mat<-matrix(c(3,1,6,9,4,2,6,8,9),nrow=3,ncol=3)         # the original matrix (example)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

M<-makeCacheMatrix(Mat)

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

InvMat<-cacheSolve(M)
InvMat          # the inverted matrix
