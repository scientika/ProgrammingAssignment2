## A set of two functions that cache matrix object and return inverse matrix using cached data if available

## a Cache Matrix Object that caches the Matrix and the computed Inverse Matrix
makeCacheMatrix <- function(invertible_mtrx = matrix()){
	InvMtrx <- NULL
	set <- function(y) {
		invertible_mtrx <<- y
		InvMtrx <<- NULL
		}
	get <- function() return(invertible_mtrx)
	setinv <- function(InvOfMtrx) InvMtrx <<- InvOfMtrx
	getinv <- function() return(InvMtrx)
	return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
}

## cacheSolve function returns a matrix that is the inverse of invertible_matrix
## it fetches data from cache if pre computed inverse exists
## else it computes the inverse and saves it to cache
cacheSolve <- function(invertible_mtrx, ...) {
	InvMtrx <- invertible_mtrx[4] #List item [4] is getinv, other subsetting methods as in class example return an error "$ operator is invalid for atomic vectors"
	if(!is.null(InvMtrx)) {
		message("Pre computed inverse exists, fetching from cache")
		#return cached inverse
		return(InvMtrx)
	}
	#precomputed value did not exist, hence will compute and set it now
	#for future cache retrieval of inverse matrix
	CurrMtrx <- invertible_mtrx$get()
	InvMtrx <-solve(CurrMtrx, ...)
	invertible_mtrx[3](InvMtrx)
	#return freshly computed inverse matrix
	return(InvMtrx)
}