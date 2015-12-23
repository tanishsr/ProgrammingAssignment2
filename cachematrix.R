## Function returns the cached version of the inverse of matrix, if inverse is found in cache, otherwise it calculates the inverse and store in the cache
## Sample Output
## Created a Identity matrix of 6000x6000 dimensions
## > mat1 <- makeCacheMatrix(diag(6000))
## Run 1: 
##	> system.time(cacheSolve(mat1))
##		user  system elapsed
##		70.520   6.455 157.461
## Run 2:
##	> system.time(cacheSolve(mat1))
##		matrix found in cache...returning
##		user  system elapsed
##		0.001   0.002   0.027

## This function accepts the matrix as argument and returns list of functions associated with the input matrix. The associated functions are updating the matrix, caching the inverse of matrix, and getting the matrix.

makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL
	set <- function(y) {
			x <<- y
			mat_inv <<- NULL
	}
	data <- function() x
	update_inverse <- function(inv) mat_inv <<- inv
	inverse <- function() mat_inv
	list(set = set, data = data,
		 update_inverse = update_inverse,
		 inverse = inverse)
}


## This function calculates the inverse of matrix. The arguement matrix should be the output of makeCacheMatrix function. If the inverse of matrix is already calculated, then the cached version of inverse is returned, otherwise the inverse is calculated and the cache is updated. It also checks for the input argument. If argument is not of type list (matrix returned from makeCacheMatrix), then it returns a message. If the input argument is of type matrix but not a return of makeCacheMatrix, then also it returns NULL.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	if(class(x) == "list")
	{
		if(is.null(x$inverse))
		{
			message("input matrix is not returned by makeCacheMatrix")
			return(NULL)
		}
		else
		{
			inv <- x$inverse()
			if(!is.null(inv))
			{
				message("matrix found in cache...returning")
				return(inv)
			}
			orig_matrix <- x$data()
			inv <- solve(orig_matrix)
			x$update_inverse(inv)
			inv
		}
		
	}
	else
	{
		message("input argument is not a valid matrix(output of makeCacheMatrix)")
		return(NULL)	
	}
}
