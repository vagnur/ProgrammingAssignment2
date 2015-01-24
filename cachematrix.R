## ENGLISH
## This document present the two functions requierd for the second assigment.
## First function is used to create a especial type of matrix data and the second one is used to operate over that matrix

## ESPAÑOL
## Este documento presenta las dos funciones requeridas en la tarea.
## La primera función crea un tipo de matriz especial y la segunda es utilizada para operar sobre dicha matriz

## ENGLISH
## This function create a especial type of matrix wich have cache on it. That means that is cappable of calculate some data just one time
## and if it need to calculate the same data, it can retrive it from the cache instead of calculate all over again.

## ESPAÑOL
## Esta función crea un tipo de matriz especial, la cual contiene la matriz y datos de cache. Gracias a esto se tiene la capacidad de calcular
## datos una sola vez, si se necesitan calcular los datos nuevamente la función rescatará esos datos de la cache y nos los volverá a calcular.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## Here the function set the matrix to a new cache variable called y
        ## and set the inverse cached matrix called i to null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## To get the matrix
        get <- function() x
        ## To set the inverse cached matrix i to the inverse variable gived to the function
        setinverse <- function(inverse) i <<- inverse
        ## To get the inverse of the matrix. Here is the trick, if the inverse is already calculated it meaans that i != NULL, so it will return the inverse of the matrix x.
        ## In any other case it will return a null value.
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## ENGLISH
## This function calculate the inverse of a matrix. If the inverse is already calculated it just take it from the cache memory and dont calculate it all over again.

## ESPAÑOL
## Esta función calcula la inversa de una matriz. Si la inversa fue calculada con anterioridad, no la calcula nuevamente la inversa y la rescata desde
## la memoria cache.

cacheSolve <- function(x, ...) {
        ## Obtain the inverse, it can be NULL or it can be the cached inverse
        i <- x$getinverse()
        ## If the variable i != NULL then it means that the inverse it has been already calculated, and it just need to ve rescued from the cache memory
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## In any other case we need to calculate the inverse of the matrix, so we get the matrix and then we call the function solve to calculate the inverse of the matrix  x
        data <- x$get()
        i <- solve(data, ...)
        ## The calculated inverse of the matrix x is saved in the variable i
        x$setinverse(i)
        ## The inverse of the matrix is returned
        i
}