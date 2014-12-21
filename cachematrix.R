## Created by: Krishnamorrthy.R
## Date: 12th Dec 2014
## Programming Assignment 2 for the R Programming course

## Matrix Inversion is usually a costly operation and it will be worth
## to cache that operation instead of computing it only when required

## This R file contains two functions called makeCacheMatrix which creates
## a special matrix object that can cache its inverse and cacheSolve that will 
## retrive the cached inverse matrix from the cache if the Matrix has not
## changed.

## The makeCacheMatrix function is to create the special matrix object which 
## can cache its inverse, It creates a list containing functions to:
## - set the value of the matrix (using the <<- operator to store the results
##   in a different environment than the current envoironment)
## - get the value of the matrix
## - set the value of matrix inverse
## - get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function is to check for the availability of the inverse matrix
## in the cache and retieve it if it is available. Otherwise the function 
## simply calculates the inverse matrix by using the solve() function and 
## stores the result in the cache using the setinverse function. Finally it
## returns the calculated inverse of the matrix

## the function does not validate for the invertiblity of the matrix. It is
## assumed that the passed matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) { ##if the i 
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# ## Test & Results
# ## create a Matrix that is 3x3 size and is invertible
# > A <- matrix(c(1,1,4,0,3,1,4,4,0),ncol=3,nrow=3)
# 
# ## print A to verify
# > A
#      [,1] [,2] [,3]
# [1,]    1    0    4
# [2,]    1    3    4
# [3,]    4    1    0
# 
# ## call the makeCacheMatrix and pass A to create the object with
# ## get set methods (functions)
# 
# > mm <- makeCacheMatrix(A)
# 
# ## use the get method to see the matrix is stored rightly
# > mm$get()
#      [,1] [,2] [,3]
# [1,]    1    0    4
# [2,]    1    3    4
# [3,]    4    1    0
# 
# ## Now call the cacheSolve method to invert the matrix
# ## since it is the first time, the message "getting cached data" should
# ## not appear
# > cacheSolve(mm)
# 
#             [,1]        [,2]    [,3]
# [1,]  0.08333333 -0.08333333  0.2500
# [2,] -0.33333333  0.33333333  0.0000
# [3,]  0.22916667  0.02083333 -0.0625
# 
# ## Now, call the same function again - this time the message "getting
# ## cached data" will appear
# 
# > cacheSolve(mm)
# 
# getting cached data
#             [,1]        [,2]    [,3]
# [1,]  0.08333333 -0.08333333  0.2500
# [2,] -0.33333333  0.33333333  0.0000
# [3,]  0.22916667  0.02083333 -0.0625
# 
# End of cachematrix.R file --