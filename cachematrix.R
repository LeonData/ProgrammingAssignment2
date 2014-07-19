# Hello, I'm Leon and it's been fun working on this assignment 

# Description of the functions in this file:
#
# Two functions 'makeCacheMatrix' and 'cacheSolve', described
# in ore details below, work in tandem and show how a result of
# a long operation that is required in several places of the code
# can be cached and returned as needed. In this case a long 
# operation is finding an inverse of a martix.
# Also, a unit test 'unitTest' is provided at the end of the file 
# that shows how to use the functions and tests their correctness.


# Function 'makeCacheMatrix':
#
# The function gets a matrix x as an input, stores (caches) it 
# as an internal variable and then provides methods 'get','set',
# 'getInverse', and 'setInverse'that respectively return 
# the original matrix, set a new one, returned an inverse of 
# the last input, or set a new inverse value.
# The function returns a function object that can be used to call
# those methods. The object is then used by the next function as
# described below.
# Let's assume that one calls this function as follows:
#
# cm <- makeCacheMatrix(x)
#
makeCacheMatrix <- function(x = matrix()) {
    # Set initial cached value to NULL
    cachedInverse <- NULL
    
    # Set function 
    # - sets a new input matrix
    # - nullifies cached inverse value
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    # Get function 
    # - returns last input matrix
    get <- function() x
    
    # Set inverse value 
    # - sets cached inverse value to one passed  
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # Get inverse function 
    # - returns cached inverse value  
    getInverse <- function() cachedInverse
    
    # A list of available functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Function 'cacheSolve':
#
# This function shall be called with the object which is
# the output of 'makeCacheMatrix'. The function returns 
# the inverse of the input matrix cached by 'makeCacheAmtrix'. 
# For example, continue our comment for 'makeCacheMatrix'
# function 'cacheSolve' shall be called as follows:
#
# inverse <- cacheSolve(cm)
#  
# To test the output one can call the matrix multiplicatio
# method as follows:
#
# diag <- x %*% inverse
#
# where x is the input matrix passwed to 'makeCacheMatrix'.
# If the function work correctly, 'diag' shall be a diagonal
# matrix (with ones on a diagonal, and very small numbers in
# all other positions). See explanation of the unit test below.
#
cacheSolve <- function(x, ...) {
    # Get cached inverse value
    invVal <- x$getInverse()
    
    # Check cached inverse value, 
    # and if it's been set - return it
    if(!is.null(invVal)) {
        message("Returning cached inverse matrix")
        return(invVal)
    }
    
    # No cached inverse value available;
    # retrieve cached data... 
    data <- x$get()
    # ... and then calculate the inverse
    # using Solve() function
    invVal <- solve(data)
    
    # Cache calculated inverse value
    x$setInverse(invVal)
    
    # Return calculated inverse value
    invVal
}

# Below I provide a unit test for these functions that both
# shows how to use them and also proves that the inverse is
# calculated correctly.
#
# Unit test first generates a random 5x5 matrix, and then calls
# bith function to get an inverse, and finally multiplies the
# inverse and the original matrix to see if the result would be 
# a diagonal matrix.
#
# When I ran 'unitTest' once and didn't do the rounding at the end,
# I got the following results:
#
# [,1]          [,2]          [,3]          [,4]          [,5]
# [1,]  1.000000e+00  1.110223e-16 -1.110223e-16  9.714451e-17 -1.110223e-16
# [2,] -2.272488e-16  1.000000e+00 -1.346145e-15  3.369700e-16  1.717376e-16
# [3,]  3.330669e-16 -2.220446e-16  1.000000e+00 -8.326673e-17 -2.220446e-16
# [4,]  0.000000e+00  4.440892e-16  0.000000e+00  1.000000e+00  0.000000e+00
# [5,]  0.000000e+00 -4.440892e-16  0.000000e+00 -8.326673e-17  1.000000e+00
#
# It can be seen that the output is almost a diagonal martx. If values are
# rounded up to 10 digits (it can be rounded up to 15 digits as well), 
# it would become a perfect diagonal. so I added the rounding at the end.
unitTest <- function(){
    # Create a sample random matrix 5x5
    m <- matrix(sample(1:100, 25, replace=T), nrow=5, ncol=5)
    
    # Call makeCacheMatrix
    mc <- makeCacheMatrix(m)
    
    # Calculate m's inverse
    minv <- cacheSolve(mc)
    
    # Multiply m and minv to see if the result will be a diagonal
    # matrix with all ones
    res <- m %*% minv
    
    # Round values up to 10 digits! If you don't do rounding,
    # the output matrix will have very small non-zero values in 
    # non-diagonal places, like 1.0123e-17.
    # You can see this by commenting the next line
    res <- matrix(lapply(res, round, 10), ncol=5, nrow=5)
    
    # return res
    res
}
