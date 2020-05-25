# The aim of these functions are to come up with a way to cache the inverse of a matrix.
# We will first create a function makeCacheMatrix that is able to cache matrices, and we will first use that to store 'special matrices'.
# Then we will create a function that will solve the inverse of a matrix formed in the makeCachematrix.
# The first time the inverse will be calculated and will be stored as a result of one of the sub functions of the makeCacheMatrix function.
# Now if we run it again (assuming the initail cached matrix does not change) the inverse will already be stored and not have to be recalculated.

# This first function makeCacheMatrix takes as its input a matrix. In reality it is a list of 4 sub functions.
# The first subfunction is set, which enables you to set the value of the matrix.
# The second subfunction is get, which outputs the value that you created in the set function. Initially its output is the matrix that is inutted as a parameter. 
# Similar to the first subfunction, the third subfunction is setinverse, which allows you to set a value of an 'inverse'. Technically, you can set this to anything and it does not have to be the actual inverse of the original matrix.
# The final subfunction is getinverse, which just outputs the value you created in setinverse. Initially its value is NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# CacheSolve takes as its input a 'special matrix' formed from the makeCacheMatrix function above.
# It first checks the result of getinverse which is the subfunction of makeCacheMatrix. Usually the inverse would not have been calculated initially and so the value of getinverse would be NULL.
# If the value is not NULL, then the cacheSolve function will produce the value that the getinverse function does produce.
# If the value is NULL (usually the first time) then the cacheSolve function will find the inverse of the 'special matrix'.
# It will then use the 'setinverse' function to set the value of the 'getinverse' function to be the actual inverse.
# This means that from now on, the value of the getinverse function is actually the inverse, so if the cacheSolve function is called again, the inverse will already be stored and so would not need to be recalculated, saving computation time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}

