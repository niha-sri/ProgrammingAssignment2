##makeCacheMatrix() creates special matrix object which on calling
##cacheSolve()can be inversed
##In case the inverse has already been calculated,
##the function would return the inverse from the cache
##else would calculate from the scratch.


makeCacheMatrix <- function(x = matrix()) {
        ##initializing the value of the object; will be used to hold inverse matrix
	i<- NULL

	##method to set the value of the matrix
	set<- function(y){
		x<<- y	
		i<<- NULL
	}
	
	##method to get the matrix
	get<- function(){
		mat
	}

	##method to set the inverse
	set_inv<- function(inverse){
	i<<- inverse
	}

	##method to get the inverse of the matrix
	get_inv<- function(){
	i
	}

	##returning a list of methods
	list(set=set, get=get, set_inverse=set_inv, get_inverse=get_inv)

}

cacheSolve <- function(x, ...) {
        ##returns value of inputted matrix
	mat<- x$get_inv()
	
	## return the invered matrix in case it's already set
	if(!is.null(mat)){
		message("Retriving from cache")
		return(mat)
	}

	## calculating matrix inverse when not present in cache
	else{
		mm<- x$get()
		mat<- solve(mm)%*% mm
		x$set_inv(mat)
		return(mat)
	}
}
