## Put comments here that give an overall description of what your functions do

# makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

   i <- NULL # initialization of i
   set <- function(y = matrix()) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInvers <- function(invers) i <<- invers
   getInvers <- function() i
   # when calling "invert", the inverse of the matrix is calculated and returned
   # a singular matrix can not be inverted
   # a square matrix A is singular if det(A) = 0
   list(set = set, get = get, setInvers = setInvers, getInvers = getInvers)
}


## cacheSolve(x) checks if I have already calculated the invers;
## if yes, it just returns it,
## otherwise it calculates the invers through "solve(x)", it assigns it to "i" and it returns it

cacheSolve <- function(x, ...) {
   i <- x$getInvers()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   A <- x$get()
   i <- solve(A, ...)
   x$setInvers(i)
   i
}

myMatrix <- makeCacheMatrix( matrix(c(1,2,30,4,5,6,70,8,9,1,110,12,13,140,15,16), nrow = 4, ncol = 4) )