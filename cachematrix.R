## The following two functions will be used to take inverts of matricies and provide caching abilty


## The first function makeCacheMatrix() will take a given matrix as an input and do the following
## 1. set the value of the matrix [function set()]
## 2. get the value of the vector [function get()]
## 3. set the value of the inverse [function set_inverse()]
## 4. get the value of the inverse
## The output of makeCacheMatrix() will be a list containing each function as an item on the list



makeCacheMatrix <- function(x = matrix()) {
        #first we start with creating the value of the inverse, by default a value of NULL
        inv <- NULL
        #then we create a function called set(), which sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #then we get the value of the matrix
        # (note we are calling x which was affirmed by the function set on line 18)
        get <- function() x
        #we then set the value of the inverse 
        set_inverse <- function(solve) inv <<- solve
        #we get the inverted matrix, (stored as inv and assigned a value in the cacheInv function)
        get_inverse <- function() inv
        
        #Output the functions in list form as explained in lines 4-9
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}



## cacheInv() does the actual computation for a matrix inversion using the solve() function
## This function is to be run AFTER makeCacheMatrix()
## This is because the 4 functions nested within there are referenced below

cacheInv <- function(x, ...) {
        #we take the existing value assigned to the variable inv
        inv <- x$get_inverse()
        #we check if inv is NULL (remember in line 15 it is set to NULL by default)
        #if inv is NOT NULL (an inverse exists), the inverse value is returned!
        #no additional computation is done and this effectively caches the inv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if inv IS NULL, we have to calculate the inverse
        #first we should retrieve the data of the matrix
        #this is done by using the get function from makeCacheMatrix() and assigning to data
        data <- x$get()
        
        #then we use the solve function to determine the inverse and assign the value to inv
        inv <- solve(data, ...)
        #we then take the inverse inv, and call the set_inverse function from makeCacheMatrix()
        x$set_inverse(inv)
        #finally we output the matrix inversion
        inv
}



#Bellow is a test cases to check if inversion works, using matrix m1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), 2, 2)
#identity matrix for reference
identityMatrix <- matrix(c(1,0,0,1),2,2)

#cache m1, assign to a
a <- makeCacheMatrix(m1)
#put a though cachesolver, assign it to b
b <- cacheInv(a)
#do matrix multiplication (%*% oeprator) of m1 and inverse (stored as b) and store it as c
c <- m1 %*% b

#check if c = stored identityMatrix, if so then message appears!
if(identical(c,identityMatrix)){message("Congrats the functions work!")
}
