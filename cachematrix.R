#############
############# I am taking "new_obj_matrix" as the matrix name that has been created by makeCacheMatrix() function


## makeCacheMatrix() function returns a list of 4 functions, whenever an invertible square matrix is passed to it.

#### Once the makeCacheMatrix() function is called, the list functions setmatrix, getmatrix, setinverse, getinverse can be operated with the
#### Object matrix passed in the makeCacheMatrix() function.
#### For example makeCacheMatrix(matrix(1:4, 2,2) is called over new_obj_matrix.
#### then new_obj_matrix$ can be used with the functions setmatrix, getmatrix, setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {
        # m_invsr-- is the object storing the inverse of the matrix
                m_invsr <- NULL
        
        # First Function for setting the Matrix object and Inverse Object (as null)
                
                setmatrix <- function(y) {
                   x <<- y
                   m_invsr <<- NULL
                 }
        
        
        # Second function to get the matrix object "x" 
                
                getmatrix <- function() x
        
        
        # third function to set inverse of the Matrix "x", this is important because caching is done here
                
                setinverse <- function(inverse) m_invsr <<- inverse
        
        
        # fourth function to get the inverse of the matrix object "x"
                
                getinverse <- function() m_invsr
        
        
        # finaaly the list of function to be returned when the function makeCacheMatrix will be called  
        
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
        
        # The inverse of the matrix is pulled from the function getinverse()
                m_invsr <- x$getinverse()
        
                ## the value of m_invsr object is tested against NULL,
                ## if not null then cached value will be returned.
                
                        if(!is.null(m_invsr)) {
                                 message("getting cached inverse")
                         return(m_invsr)
                         }
                
                ## else the inverse of the matrix will be computed here, in steps 
                        matrix_passed <- x$getmatrix()  #--->here the matrix object is retrived
                
                        m_invsr <- solve(matrix_passed, ...) #---> over here the inverse is calculated
        
                        x$setinverse(m_invsr) # ---> here the m_invsr object is cached by the function 'setinverse'
                
        
        # finally the inverse is computed and returned 
        m_invsr
}