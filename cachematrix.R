## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # 1. initialize the cache Matrix
  # assign the value NULL 
  
  cacheM <- NULL
  
  # 2. define the method  'setM'
  
  setM <- function(y) {
    x <<- y
    cacheM <<- NULL
  }
  
  # 3. define the method named 'getM' return the matrix 'x'
  
  getM <- function() x
  
  # 4. define the method named 'setC'
  
  setC <- function(inverse) cacheM <<- inverse
  
  # 5. define the method named 'getC' that returns the cached inverse of 'x'
  
  getC <- function() cacheM
  
  # 6. list the names of all methods 
  list(setM = setM,
       getM = getM,
       setC = setC,
       getC = getC)
  
  #---------------------------------------------------------
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # 1. check the content of cache matrix
  
  cacheM <- x$getC()
  
  # 2. if the content is not null then: return the result 
  
  if (!is.null(cacheM)) {
    message("loading cache matrix...")
    return(cacheM)
  }
  
  # 3. if the content is empty then: get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getM()
    cacheM <- solve(dMatrix, ...)
    x$setC(cacheM)
    return(cacheM)
  }
  #---------------------------------------------------------
  
}

## Testing the functions
My_Matrix <- makeCacheMatrix(matrix(0:8, 8, 8))
My_Matrix$getM()
My_Matrix$getC()
cacheSolve(My_Matrix)
My_Matrix$getC() 
