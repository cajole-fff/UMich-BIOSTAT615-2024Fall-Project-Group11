## Generate sparse matrix (can use nrow=2 for row dist func)
## S is sparsity
rgen_sparse = function(nrow, ncol, S) {
    thres = qnorm(S) #threshold which prop'n S of data falls under 
    vals = rnorm(nrow*ncol)
    vals[which(vals < thres)] = 0 #values that fall under threshold = 0
    return(matrix(vals, nrow=nrow))
}

## Convert standard dense matrix to sparse in CSR format

library(Matrix)

dense_to_CSR = function(X){
    n = nrow(X)
    m = ncol(X)
    non0_index = which(X != 0)
    i_indices = (non0_index-1) %% n + 1 #1-based index
    j_indices = (non0_index-1) %/% n + 1
    
    return (sparseMatrix (
        i = i_indices, 
        j = j_indices, 
        x = X[non0_index], 
        dims = c(n, m), 
        repr = "R"
        ))
}

## Find minkowski distance using standard method
Mdist = function (v1, v2, p) {
    return (sum(abs(v1 - v2) ^ p) ^ (1/p))
}
