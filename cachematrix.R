## A pair of functions that act together to compute and 
##  cache the inverse of a matrix.  This avoids computational
##  performance issues with retrieving the inverse of a matrix
##  if the contents of the matrix has not changed.




## makeCacheMatrix(x)
##  Creates a matrix holding object to store both the 
##  matrix and its calculated inverse
##
## x <- A valid matrix to be stored.
##
## returns: A list of functions.
##          get() returns the original matrix
##          set() updates the stored marix and clears out the previously stored inverse
##          getInv() returns the calculated invers of the matrix previously stored
##          setInv() stores the inverse of the matrix for retrieval later.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  #inverse initially set to NULL
    
    ##function to store a new matrix
    set <- function(y){
        x <<- y 
        i <<- NULL  #clear out stored inverse
    }
    
    ##function to get stored matrix
    get <- function() x 
    
    ##function to set stored(cached) matrix
    setInv <- function(z){
        i <<- z    
    } 
    
    ##function to get cached matrix
    getInv <- function() i
    
    #return the list of internal functions available
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}




## cacheSolve(x,...)
##  Computes the inverse of the matrix stored in an object
##  return from makeCacheMatrix().  

cacheSolve <- function(x, ...) {
    #get stored inverse
    inv <- x$getInv()
    
    #if already cached, just return
    if(!is.null(inv)){
        message("getting cached inverse data")
        return(inv)
    }
    
    #retrieve the matrix
    mat <- x$get()
    
    #calculate inverse
    inv <- solve(mat,...)
    
    #store
    x$setInv(inv)
    
    #return
    inv
}
