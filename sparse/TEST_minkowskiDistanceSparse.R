source("minkowskiDistanceSparse.R")
source("TEST_minkowski.R")

## Helper function to test my implementation 
## Generate sparse rows, convert to sparse, test distance both ways & compare
## Repeat n times & return proportion correct
testMinkowskiDistanceSparse = function (n, p) {
    n_correct = 0
    for (i in 1:n) {
        test_mat = rgen_sparse(2, 100, .9)
        mdist1 = Mdist(test_mat[1, ], test_mat[2, ], p) #naive dist func
        
        ## Convert to CSR and run using sparse matrix func
        X = dense_to_CSR(test_mat)
        mdist2 = minkowski_distance_sparse(X, 1, 2, p = p)
        if (mdist1 != mdist2) {
            print("Expected:")
            print(mdist1)
            print("You got:")
            print(mdist2)
            print("Absolute Error:")
            print(abs(mdist1 - mdist2))
        }
        n_correct = n_correct + (mdist1 == mdist2)
    }
    return (n_correct / n)
}

testMinkowskiDistanceSparse(10, 20)
