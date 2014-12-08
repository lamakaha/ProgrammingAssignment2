##These 2 functions allow to cache the computation of a matrix inverse 
##and retrieve the pre-computed value on subseqent attempts to calculate the inverse
##on the same matrix. If the matrix is updated, the cache gets reset
##Usage example:
##mat1<-matrix(sample(1:9, 9), 3) #create regular matrix
##cachemat1<-makeCacheMatrix(mat1) #create special cach-enabled matrix
##cacheSolve(cachemat1) #this will calculate the inverse since the cache is empty
##cacheSolve(cachemat1) #subsequent calls would retrieve cached version of the inverse

##This function creates a special cache enabled matix which exposes 4
##methods for setting/getting the matrix and the corresponding cached inverse
##The set function updates the matrix and resets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv<-NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) x_inv <<- inv
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


##This function works in conjuction with makeCacheMatrix function
##It calculates an inverse matrix for the special cache-enabled matrix it receives
##unless the matrix already has a pre-calculated inverse in its cache
##in which case it only retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv   
}
