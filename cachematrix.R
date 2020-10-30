## Write a short comment describing this function

#This function gets the inversion of a SQUARE MATRIX on its cache
#it set  the matrix, get the matrix values,
#set the inverse of the matrix 
#and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL           #the inverse is the
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x  # get is the function to get the matrix
     setinverse <- function(solve) inverse <<- solve 
     getinverse <- function() inverse # function that give us the inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

##function that calculate the inversion of a matrix that has been
##created with the function above. If the inversion is on the cache
##the computation is skipped. If it is not, the inverse is 
#calculated via the set inverse function

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     if(!is.null(inverse)) {  #check if the inverse is NULL
          message("getting cached data")
          return(inverse) #return the inverse matrix
     }
     data <- x$get()
     inverse <- solve(data, ...) ## calculates the inverse value
     x$setinverse(inverse)
     inverse          ## return a matrix inverse of x
}
