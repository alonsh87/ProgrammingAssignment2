## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  
  sol<-NULL
  
  set<-function(y)
    {
    x<<-y
    sol<<-NULL
    }
  get.mtrx<-function() x  # returns the matrix
  set.inv<-function(inv) sol<<-inv # sets the inverse matrix in cache
  get.inv<-function() sol # retruns the inverse
  
 
  return(list(set=set,get.mtrx=get.mtrx,get.inv=get.inv,set.inv=set.inv))

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
       
 inverse<-x$get.inv() # getting the inverse from makeCacheMatrix
 
 if(!is.null(inverse))  # if it's a value, returns that value from cache
   {
  message("getting cached data")
  return(inverse)
   }
 inverse<-solve(x$get.mtrx()) # if it's NULL it gets the inverse of the matrix from 
                              # makeCacheMatrix
 x$set.inv(inverse)       # sets the inverse in the cache 
 
 return (inverse)         # returns the inverse
 
  
}
