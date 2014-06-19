## Coursera - Juin 2014
## R Programming - Rprog-004
## Ces deux fonctions servent à calculer l'inverse d'une matrice et la mettre en cac

## Cette fonction permet de mettre une matrice en cache

makeCacheMatrix <- function(x = matrix()) {
## Creates a list of functions that
  ## can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cette fonction permet de calculer l'inverse d'une matrice. Si l'inevrse existe en cache, 
## la fonction renvoi les valeurs du cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

## Computes the inverse of the matrix returned
  ## by makeCacheMatrix(), unless the inverse has
  ## already been calculated, in which case
  ## it retrieves it from the cache.
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
