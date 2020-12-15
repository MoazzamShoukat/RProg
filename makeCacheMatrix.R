makeCacheMatrix <- function(x = matrix()){
      in_verse <- NULL
      set <- function(y){
            x <<- y
            in_verse <<- NULL
      }
      get <- function() {x}
      setInv <- function(in_verseerse) {in_verse <<- inverse}
      getInv <- function() {in_verse}
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacSolve <- function(x, ...){
      in_verse <- x$getInv()
      if(!is.null(in_verse)){
            message("Retreiving Data from Cache")
            return(in_verse)
      }
      mat <- x$get()
      in_verse <- solve(mat, ...)
      x$setInv(in_verse)
      in_verse
}
