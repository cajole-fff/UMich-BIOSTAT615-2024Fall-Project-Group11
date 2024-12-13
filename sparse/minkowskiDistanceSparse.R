
## Find Minkowski distance between two sparse matrices
# Load the required package
library(Matrix)

# Minkowski Distance Function
#' minkowski_distance_sparse(): calculate Minkowski distance between two rows
#' given in CSR format
#' @param A : Matrix in Compressed Sparse Row (CSR) representation
#' @param r1,r2 : 1-based indices of rows we want to compare
#' @param p : Minkowski distance parameter (default 2)
#' @return numeric value of distance between both rows
minkowski_distance_sparse <- function(A, r1, r2, p = 2) {
    ## If finding dist b/t row and self, return 0
    if (r1 == r2) {
        return (0)
    }
    
    c = A@j #extract column indices
    v = A@x #extract values
    rp = A@p #extract row pointers
    
    ## Distance between two all-zero rows is zero
    ## Distance between any row and all-zero row is Min. sum of former row
    ## For two non-empty rows, more care is taken
    if ((rp[r1] == rp[r1 + 1]) & (rp[r2] == rp[r2 + 1])) {
        return (0)
    } else if (rp[r1] == rp[r1 + 1]) {
        ## Find beginning and ending indices (1-based!) for row 2
        r2_idx = (rp[r2] + 1) : rp[r2 + 1]
        c2 = c[r2_idx] + 1 #convert to 1-based
        v2 = v[r2_idx]
        print("Called Case II")
        print(v2)
        return (sum(abs(v2)^p)^(1/p))
    } else if (rp[r2] == rp[r2 + 1]) {
        ## Find beginning and ending indices (1-based!) for row 1
        r1_idx = (rp[r1] + 1) : rp[r1 + 1]
        c1 = c[r1_idx] + 1 #convert to 1-based
        v1 = v[r1_idx]
        print("Called Case III")
        print(v1)
        return (sum(abs(v1)^p)^(1/p))
    } else {
        ## Find beginning and ending indices (1-based!) for both rows
        r1_idx = (rp[r1] + 1) : rp[r1 + 1]
        r2_idx = (rp[r2] + 1) : rp[r2 + 1]
        c1 = c[r1_idx] + 1 #convert to 1-based
        v1 = v[r1_idx]
        c2 = c[r2_idx] + 1 #convert to 1-based
        v2 = v[r2_idx]
        
        dist_count = 0 #initialize
        nonzeroes = intersect(c1, c2) #indices with nonzero values in both rows
        ## First, take care of nonzero values in both rows
        ## If none, this loop will be bypassed entirely
        while (length(nonzeroes) > 0) {
            common_idx = nonzeroes[1]
            nonzeroes = nonzeroes[-1] #remove first entry
            ## Find corresponding value at both entries
            common_idx1 = which(c1 == common_idx)
            common_idx2 = which(c2 == common_idx)
            A_val = v1[common_idx1]
            B_val = v2[common_idx2]
            ## Remove corresponding indices & values
            c1 = c1[-common_idx1]
            c2 = c2[-common_idx2]
            v1 = v1[-common_idx1]
            v2 = v2[-common_idx2]
            dist_count = dist_count + abs(A_val - B_val) ^ p
        }
        ## Min. sum the remaining values
        return ((sum(abs(v1) ^ p) +
                 sum(abs(v2) ^ p) +
                 dist_count) ^ (1 / p))
    }
}

# Minkowski Distance Function
#' compute_distance_matrix_sparse(): calculate Minkowski distance between each 
#' pair of rows of a sparse matrix
#' @param A : sparse matrix in CSR format with n columns
#' @param p : Minkowski distance parameter (default 2)
#' @return Distance matrix in standard base R matrix format with dimensions 
#' nrow(A) x nrow(B). Entry (i,j) is the distances between row i of matrix A 
#' and row j of matrix B.
compute_distance_matrix_sparse <- function(A, p = 2) {
    ## Function wrapper for Minkowski distance
    MDist_wrapper <- function(r1, r2) {
        MDist_Vec = Vectorize (
            FUN = minkowski_distance_sparse,
            vectorize.args = c("r1", "r2"))
        return(MDist_Vec(A = A, r1 = r1, r2 = r2, p = p))
    }
    ## Generate all possible row indices and call outer()
    allRows = c(1:(A@Dim[1])) #second Dim entry is nrow(A)
    return (outer(allRows,
                  allRows, 
                  FUN = MDist_wrapper))
}
