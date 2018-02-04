## Put comments here that give an overall description of what your
## functions do


##These functions were created to address the Coursera R-programming assignment requirements on 04-Feb-2018, by Bagdat Toleubay (gitHub: btoleubay90).
##The main body of the requirement has been forked from rdpeng(URL: https://github.com/rdpeng/ProgrammingAssignment2) on 04-Feb-2018.  
##The functions below receive a matrix and compute an inverse of the matrix (assuming the matrix supplied is invertable). Once the operation is complete, instead of solving for the inverse next time for the same matrix, it is withdrawn from the cache. Usefull for looping functions.
## The process consists of two functions: makeCacheMatrix and cacheSolve

##First function: makeCacheMatrix. 
##The structure of the function closely resembles the one supplied in the assignment description.
##reads the input matrix and creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function (mx=matrix()) {
  inv <-NULL
  set <- function(y){
    mx <<- y
    inv <<- NULL
  }
  get <- function() mx
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get= get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##The second function: cacheSolve
##Given the function-previous had been run, function below computes the inverse. Before computation starts, the function sees if the inverse been calculated. If yes, it retrieves the inverse from the cache and skips computation. If does not find the matrix inverse in the cache, it continues witht the computation. 

cacheSolve <- function (mx, ...) {
  inv <- mx$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mx$get()
  inv <- solve(data, ...)
  mx$setinverse(inv)
  inv
}
