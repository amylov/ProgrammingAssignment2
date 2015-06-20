## https://github.com/amylov/ProgrammingAssignment2.git
## This R programming assignment is regarding lexical scoping and 
## to handle the inverse of a matrix using caching. 
## By having caching, the inverse of a matrix need not be computed
## repeatedly as matrix inversion is a costly computation. 

## The makeCacheMatrix function creates a special "matrix" object 
## which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
    set <- function(y){
        x <<- y       ##superassignment operator
        m <<- NULL    ## Variable m was declared and initialised by makeCacheMatrix.
                      ## So need to update the value of variable m
    }
    get <- function()x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## which was created with the makeCacheMatrix function.
## First it checks to see if the inverse has already been calculated.
## If yes, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via setinverse function.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
              message("getting cached data")
              return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## Note that variable m is declared uniquely in both functions
## and are allocated separate addresses in memory. 

## This is the test script to proof the return of the cache value for the inverse matrix.  

## Step 1 :
## > a <- makeCacheMatrix(x = matrix(1:4,2))
## > a$get()
## Output gives a 2x2 matrix

## Step 2 :
## > a$getinverse()
## Output gives NULL value

## Step 3 :
## > cacheSolve(a)
## Calculates the inversion value of the 2X2 matrix and show the output.

## Step 4 : 
## > a$getinverse()
## Output show that the inverse has been stored and does not affect anything. 
## Output give the same inversion value of the 2X2 matrix

## Step 5 :
## > cacheSolve(a)
## Output gives message "getting cached data". 
## This proof the return of the cache value for the inverse matrix.  
## Output also gives the inversion value of the 2X2 matrix

