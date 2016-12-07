## The below two functions, performs the task of finding the inverse
## of a matrix using the caching method in order to optimise the time
## consuming computations involved in this process


## The first function, makeCacheMatrix creates a list which contains 
## functions to

##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){  ## set the value of the matrix 
                x<<-y
                inv<<-NULL
        }
        
        get<-function(){x}  ## get the value of the matrix
        
        setInv<-function(i){ ## set the value of the inverse
                inv<<-i
        }
        
        getInv<-function(){inv} ## get the value of the inverse
        
        ## List containing the functions
        list(get=get,set=set,setInv=setInv,getInv=getInv)
}


## The following function calculates the inverse using the list returned 
## by the makeCacheMatrix() function

## The cacheSolve() first checks to see if the inverse had already been 
## calculated before or not. If so, then it will fetch the result from 
## the cache and skips the re-computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache 
## via the setInv().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        
        ## checks whether the inverse is already computed or not
        if(!is.na(inv)){  
                message("Cached data read")
                return(inv)
        }
        
        ## solve() :A R's function to find the inverse of a matrix
        #da<-x$get()
        inv<-solve(x$get())
        
        x$setInv(inv)
        inv
}
