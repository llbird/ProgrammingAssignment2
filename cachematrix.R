# 缓存逆矩阵

# 该函数用于创建可缓存逆矩阵的特殊“矩阵”对象
# INPUTS: (matrix) x
# OUTPUTS: (function list) set, get, setSolve, getSolve
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL #更改矩阵数据后，需要重设S
    }
    
    get <- function() x
    
    setSolve <- function(solve) s <<- solve
    
    getSolve <- function() s
    
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

# 该函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵
# INPUTS: (cached matrix) x
# OUPUTS: (inverse of cached matrix) s
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}