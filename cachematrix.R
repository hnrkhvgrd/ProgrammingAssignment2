## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: This function creates a matrix that is special like in the example with the special vector

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                        ##begin with inv = NULL
+         set <- function(y) {     ##definition of the set function
+                 x <<- y          ##value og matrix in first environment
+                 inv <<- NULL     ##whenever there is a new matrix, set inv = NULL
+         }
+         get <- function() x.     ##definition of the get function
+         setinverse <- function(inverse) inv <<- inverse ##assigns value of the inv in the first environment
+         getinverse <- function() inv ## gets the value of inv
+         list(set = set, get = get,   ## refer to the function
+              setinverse = setinverse,
+              getinverse = getinverse)
}


## Write a short comment describing this function: This function calculates the inverse of the matrix returned by the makeCacheMatrix 
## If there has already been calculated an inverse of makeCacheMatrix then cacheSolve gets the invers from the cache

cacheSolve <- function(x, ...) {
+         inv <- x$getinverse()
+         if(!is.null(inv)) {
+                 message("getting cached data")
+                 return(inv)
+         }
+         data <- x$get()
+         inv <- solve(data, ...)
+         x$setinverse(inv)
+         inv
}
