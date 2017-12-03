#####################
#GENERAL DISCRIPTION
#####################
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

#####################
#makeCacheMatrix
#####################
# This function creates a list with functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#####################
#cacheSolve
#####################
# This function returns the inverse of the matrix. It first checks if
# Check if the inverse has already been computed. 
# If yes: get the result and skip the computation. 
# If no: compute the inverse and set the value in the cache via function setinverse.

# CAUTION: This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

###########
# EXAMPLE
###########
# myMatrix = rbind(c(1, -0.5), c(-0.5, 1))
# m = makeCacheMatrix(myMatrix)
# m$get()
# [,1] [,2]
# [1,]  1.0 -0.5
# [2,] -0.5  1.0

# First run -> computation necessary
# cacheSolve(m)
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333

# Second run -> cache can be used to speed up computation
# cacheSolve(m)
# Getting cached data.
# [,1]      [,2]
# [1,] 1.3333333 0.6666667
# [2,] 0.6666667 1.3333333

# sessionInfo()
# R version 3.3.3 RC (2017-02-27 r72279)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 16.04.3 LTS
# 
# locale:
#   [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
# [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_GB.UTF-8    
# [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_GB.UTF-8   
# [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#   [1] tools_3.3.3
# Sys.time()
# [1] "2017-12-03 17:30:11 CET"