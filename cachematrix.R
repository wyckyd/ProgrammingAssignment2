#  makeCacheMatrix creates a "cacheMatrix" structure, containing: 
#    x - a matrix
#    i - cache to store the inverted matrix / the solve
#    get and set to retrieve and alter the value of x
#    getsolve and setsolve to manipulate i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # i = inverted matrix
  set <- function(y) { # set matrix x
    x <<- y             
    i <<- NULL # when setting x, reset i
  }
  get <- function() x # get matrix x
  setsolve <- function(data) i <<- data # set i to new data
  getsolve <- function() i # get the inverted matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# cacheSolve checks if the getSolve for a given cacheMatrix is in the cache. If so, it returns that value. 
# If not, it calculates the solve, and stores that in the cacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getsolve()        # get the stored solve from cache
  if(!is.null(inv_matrix)) {        # if the stored solve is not empty, 
    message("getting cached data")  # let the user know you are getting solve from cache
    return(inv_matrix)              # return the solve gotten from cache
  }                                 # otherwise...   
  m <- x$get()                      # get the original matrix from x
  inv_matrix <- solve(m)            # and solve it
  x$setsolve(inv_matrix)            # store the calculated solve in x
  inv_matrix                   # and return the calculated solve
}
