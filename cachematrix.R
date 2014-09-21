#############
############# I am taking "new_obj_matrix" as the matrix name that has been created by makeCacheMatrix() function


## makeCacheMatrix() function returns a list of 4 functions, whenever an invertible square matrix is passed to it.

#### Once the makeCacheMatrix() function is called, the list functions setmatrix, getmatrix, setinverse, getinverse can be operated with the
#### Object matrix passed in the makeCacheMatrix() function.
#### For example makeCacheMatrix(matrix(1:4, 2,2) is called over new_obj_matrix.
#### then new_obj_matrix$ can be used with the functions setmatrix, getmatrix, setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {
        m_invsr <- NULL
        setmatrix <- function(y) {
                x <<- y
                m_invsr <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m_invsr <<- inverse
        getinverse <- function() m_invsr
        list(setmatrix  = setmatrix , getmatrix = getmatrix ,
             setinverse = setinverse ,
             getinverse = getinverse )
        
}


## cacheSolve() function computes the inverse of the new_obj_matrix  (which is created by the makeCacheMatrix() function) . .

#### If the Invesre of the has not been compouted or have not been set by the new_obj_matrix$setinverse(), then
#### cacheSolve() function will be computing the invesre and then caching it in thr "m_invsr" object
#### Again , if the cacheSolve() function is called over the same new_obj_matrix, then it will be returning the Cached inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_invsr <- x$getinverse()
        if(!is.null(m_invsr)) {
                message("getting cached inverse")
                return(m_invsr)
        }
        matrix_passed <- x$getmatrix()
        m_invsr <- solve(matrix_passed, ...)
        x$setinverse(m_invsr)
        m_invsr
}