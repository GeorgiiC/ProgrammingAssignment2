## Programming Assignment 2 -- Lexical Scoping
## GeorgiiC, 31.07.2018

## Descripition
## Repeatedly calculating the inverse of a matrix tends to be very 
## time-consuming. Therefore, if the contents of a matrix do not change it is 
## sometimes advisable to cache its contents (and then look them up rather than 
## recompute them)

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix

## Inputparams: a matrix x, with dim(x)[1] == dim(x)[2]
## Outputparams: a list of functions inv_mat

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y){
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinvmat <- function(inverse_mat) inv_mat <<- inverse_mat
    getinvmat <- function() inv_mat
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


## The second function, cacheSolve calculates the inverse of the special "matrix"
## calculated with the above function. However, it first checks to see whether 
## the inverse of the matrix has already been calculated. If so, it gets the 
## the inverse of the matrix from cache and skips computation. Otherwise, it 
## calculates the inverse of the matrix and sets the inverse in the cache via 
## the setinvmat function.

## Inputparams: a matrix x, with dim(x)[1] == dim(x)[2]

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinvmat()
        if(!is.null(inv_mat)){ # if inv mat already is in the cache
            message ("getting cached data")
            return(inv_mat) # breaks out of function and returns the inverse
        }
        data <- x$get() # gets matrix
        inv_mat <- solve(data, ...) # calculates the inverse of matrix
        x$setinvmat(inv_mat) # caches the inverse
        inv_mat # returns the inverse
}

## Examples
## x <- matrix(rnorm(4), 2, 2)
## x
##    [,1]      [,2]
## [1,] 0.7980905  1.114701
## [2,] 1.3360852 -0.265131

## inv_mat <- makeCacheMatrix(x)
## inv_mat$get()
##     [,1]      [,2]
## [1,] 0.7980905  1.114701
## [2,] 1.3360852 -0.265131

## First Run:
## cacheSolve(inv_mat)
##     [,1]       [,2]
## [1,] 0.1558738  0.6553464
## [2,] 0.7855008 -0.4692071

## Second Run:
## cacheSolve(inv_mat)
## getting cached data
##      [,1]       [,2]
## [1,] 0.1558738  0.6553464
## [2,] 0.7855008 -0.4692071



