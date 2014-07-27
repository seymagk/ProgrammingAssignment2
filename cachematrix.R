## In this file, two functions are described: makeCacheMatrix and cacheSolve.
## With these two functions, you can cache the inverse of a matrix so that
## you don't have to calculate it each time. This will help saving time.

## The following function creates a matrix that can cache its inverse.
## The function takes a matrix as its one argument. It returns a list of four
## functions: get, set, getInv, setInv. 

makeCacheMatrix <- function(mtx = matrix()) {  ## Accepts a matrix as an argument.
        
        mtxInv <- NULL                         ## mtxInv is the inverse of the matrix. 
                                               ## We set it to be NULL at the beginning,
                                               ## because it hasn't been calculated yet.
                                        
        set <- function(m) {                   ## "set" function changes the value of our
                                               ## initial matrix. i.e. you define a new matrix
                                               ## without calling makeCacheMatrix again.
                mtx <<- m                      ## New value is assigned to "mtx"
                mtxInv <- NULL                 ## Important to set mtxInv to NULL, as we have
                                               ## a new matrix now. The old inverse matrix 
                                               ## shouldn't be used.
        }
        get <- function() mtx                  ## "get" function returns the matrix.
        setInv <- function(inv) mtxInv <<- inv ## After calculating the inverse matrix in a different
                                               ## environment, setInv saves this value to mtxInv.
        getInv <- function() mtxInv            ## getInv is used to get the inverse matrix.
        list(set = set, get = get,             ## The function returns a list of four functions.
             setInv = setInv,
             getInv = getInv)

}


## The following function calculates the inverse of a matrix. If the inverse has
## already been calculated, then it returns the value that is cached by 
## "makeCacheMatrix" function.
## The function takes a list as an argument. This list is what "makeCacheMatrix" 
## function returns.
## It returns the inverse of the matrix. 

cacheSolve <- function(cacheM, ...) {
        
        inv <- cacheM$getInv()                  ##"getInv" function of the "makeCacheMatrix"
                                                ## is used here to assign the value of "inv"
                                                ## which represents the inverse matrix.
        if(!is.null(inv)) {                     ## If the inverse of the matrix has been
                message("getting cached data")  ## calculated, then the function returns
                return(inv)                     ## the cached value.
        }
                                                ## If the inverse is NULL, which means that
                                                ## the inverse matrix has not been calculated,
        mt <- cacheM$get()                      
        mtinv <- solve(mt, ...)                 ## then, it is calculated here.  
        cacheM$setInv(mtinv)                    ## The inverse matrix that is just calculated
                                                ## is stored by "setInv" function of the
                                                ## "makeCacheMatrix" function. In this way, we
                                                ## will not calculate it again.
        mtinv                                   ## Return a matrix that is the inverse of 'mt'
        
}
