## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create an object matrix which will have the matrix
## and the inverse of it stored in cache
makeCacheMatrix <- function(x = matrix()) {

        #initialiing it to null
        inverseM <- NULL
        
        #function to set the matrix and cache it with its inverse
        setM <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        
        # getting the matrix from the cache if available
        getM <- function() x
        
        # setting the inverse of the matrix to the cache
        setInverseM <- function(inverse) inverseM <<- inverse
        
        # getting the inverse of the matrix from the cache
        getInverseM <- function() inverseM
        
        #list vector containing the options of setting a matrix,getting a matrix
        # and also setting and getting its inverse
        list(set = setM, get = getM, setInv = setInverseM,getInv = getInverseM)
        
}


## Write a short comment describing this function
## This function will find out if there is any cache of inverse matrix
## If not then it will find the inverse of the matrix and will store it

cacheSolve <- function(x, ...) {
         
        ## Return a matrix that is the inverse of 'x'
        
        #getting the inverse of the matrix if available
        inverseM <- x$getInv()
        
        #check if it exists then return from cache
        if(!is.null(inverseM)){
                
                print("Inverse of the matrix is cached")
                return(inverseM)
        }
        
        #get the matrix in a vector
        data <- x$get()
        
        #finding the inverse of the matrix with solve function
        inverseM <- solve(data,...)
        
        #setting the inverse of the matrix in the cached variable
        x$setInv(inverseM)
        
        #returning the inverse
        inverseM
        
}
