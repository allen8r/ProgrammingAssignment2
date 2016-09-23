## A matrix container that can cache the inverse of the matrix
## makeCacheMatrix - creates the matrix container
## cacheSolve - takes cacheMatrix created by makeCacheMatrix and returns
##              the inverse matrix
##
## Sample usage:
##
## Make a 5x5 square matrix with random integers between 1 and 100.
## > mat5x5 <- matrix(sample(1:100,25), ncol = 5)
##
## Create a cache matrix using 'mat5x5'.
## > cm5x5 <- makeCacheMatrix(mat5x5)
##
## See the matrix stored in cm5x5
## > cm5x5$get()
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   18   78   66   72   60
## [2,]   86   89   76   82   98
## [3,]   20   83   58   12   35
## [4,]   46   97   68    5   41
## [5,]   39   44   84   13   32
##
## Solve for the inverse of 'mat5x5' using the cache matrix 'cm5x5'.
## > cacheSolve(cm5x5)
##              [,1]         [,2]         [,3]         [,4]         [,5]
## [1,]  0.022488650 -0.009608382 -0.097944121  0.075751795 -0.002671154
## [2,]  0.021415146 -0.013434887 -0.040434936  0.046569612 -0.014450661
## [3,]  0.003369696 -0.005856640  0.001806377 -0.007772924  0.019601116
## [4,]  0.055525085 -0.023643079 -0.113718497  0.077147683 -0.006168466
## [5,] -0.088256385  0.055161867  0.216423836 -0.167293036  0.005428137
##
## Solve for the inverse again to see if we get the cache copy; the printed
## message indicates that the inverse matrix is retrieved from cached copy.
## > cacheSolve(cm5x5)
## getting cached inverse matrix
##              [,1]         [,2]         [,3]         [,4]         [,5]
## [1,]  0.022488650 -0.009608382 -0.097944121  0.075751795 -0.002671154
## [2,]  0.021415146 -0.013434887 -0.040434936  0.046569612 -0.014450661
## [3,]  0.003369696 -0.005856640  0.001806377 -0.007772924  0.019601116
## [4,]  0.055525085 -0.023643079 -0.113718497  0.077147683 -0.006168466
## [5,] -0.088256385  0.055161867  0.216423836 -0.167293036  0.005428137
##
## Show that the solved matrix is indeed the inverse matrix of 'mat5x5';
## i.e. A * B = I, where A is a matrix, B is its inverse, and I is the
## identity matrix.
## > identMat <- mat5x5 %*% cm5x5$getInv()
## > round(identMat)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    0    0    0
## [2,]    0    1    0    0    0
## [3,]    0    0    1    0    0
## [4,]    0    0    0    1    0
## [5,]    0    0    0    0    1


makeCacheMatrix <- function(mat = matrix()) {
    ## Creates a matrix container that caches a copy its supplied inverse
    ## args:
    ##  mat - an invertible square matrix
    iMat <- NULL
    
    set <- function(mt) {
        mat <<- mt
        iMat <<- NULL
    }
    
    get <- function() {
        mat
    }
    
    setInv <- function(invMat) {
        iMat <<- invMat
    }
    
    getInv <- function() {
        iMat
    }
    
    list(set=set, get=get, 
         setInv=setInv, getInv=getInv)
}


cacheSolve <- function(mat, ...) {
    ## Retrieves and returns a matrix inverse of 'mat'
    ## by first checking for a cached copy in 'mat'.
    ## If no cached copy exists, the inverse is calculated,
    ## stored in 'mat', and returned.
    ##
    ## args:
    ##  mat - a cacheMatrix created by makeCacheMatrix function
    iMat <- mat$getInv()
    
    if(!is.null(iMat)) {
        message("getting cached inverse matrix")
        return(iMat)
    }
    
    data <- mat$get()
    iMat <- solve(data, ...)
    mat$setInv(iMat)
    iMat
}
