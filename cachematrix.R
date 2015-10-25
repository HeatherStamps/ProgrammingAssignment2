## set: enables values from y to be used in in function when cached value of the invesere is null
## get: retrieves function with x input
## setinverse: stores the value of the inverse of x
## getinverse: retrieves inverse from cache 

x <- makeMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                
                x <<- y
                i <<- NULL
                
                return(x)}
        
        get <- function() x
        setinverse <- function(inverse)  i <<- inverse
        getinverse <- function () i
        
        
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)        
           
       
        
         }
      

        


## cachesSolve checks cache for matrices for which the inverse has been solved.
## If the inverese for the inputted value of x has not been solved for, it solves for the inverese value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } 
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        }

