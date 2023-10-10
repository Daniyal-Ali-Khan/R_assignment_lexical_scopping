# Define the makeVector function
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


# Create a new vector
v <- makeVector()

# Set the value of the vector
v$set(c(1, 2, 3, 4, 5))

# Get the value of the vector
vector_value <- v$get()
cat("Vector Value:", vector_value, "\n")

# Set the value of the mean
v$setmean(mean(vector_value))

# Get the value of the mean
mean_value <- v$getmean()
cat("Mean Value:", mean_value, "\n")
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve_matrix) {
    inverse <<- solve_matrix
  }
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- cacheMatrix$get()
  inverse <- solve(data, ...)
  
  cacheMatrix$setInverse(inverse)
  
  inverse
}

# Create a cache matrix object
matrixObj <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse of the matrix and cache it
inverseMatrix <- cacheSolve(matrixObj)

# Retrieve the cached inverse
cachedInverseMatrix <- cacheSolve(matrixObj) # This will retrieve the cached value

# You can also set a new matrix and compute its inverse
matrixObj$set(matrix(c(2, 1, 0, 3), nrow = 2))
newInverseMatrix <- cacheSolve(matrixObj)
