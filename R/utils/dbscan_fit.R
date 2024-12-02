library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/dbscan_fit.cpp"))

#' @description Fits the DBSCAN clustering model on the input data using Rcpp.
#' @param X array-like or sparse matrix of shape (n_samples, n_features).
#' The input data to be clustered.
#' @return self Returns the instance itself.

dbscan_fit <- function(
    X, # {array-like, sparse matrix} of shape (n_samples, n_features)
    eps, # {numeric} The maximum distance between two samples for one to be considered as in the neighborhood of the other.
    min_samples, # {integer} The number of samples in a neighborhood for a point to be considered as a core point.
    metric, # {string} The metric to use when calculating distance between instances in a feature array.
    metric_params, # {list} Additional arguments for the metric function.
    algorithm, # {string} The algorithm used to compute nearest neighbors.
    leaf_size, # {integer} Leaf size passed to BallTree or KDTree.
    p, # {integer} Power parameter for the Minkowski metric.
    n_jobs # {integer} The number of parallel jobs to run.
) {
    # Validate input
    if (is.null(X)) stop("Input data X cannot be NULL.")
    if (!is.matrix(X)) stop("Input data X must be a matrix.")
    
    # Call the C++ DBSCAN implementation
    result <- dbscan_fit_cpp(
        X = as.matrix(X),
        eps = eps,
        min_samples = min_samples,
        metric = metric,
        metric_params = metric_params,
        algorithm = algorithm,
        leaf_size = leaf_size,
        p = p,
        n_jobs = n_jobs
    )

    # Return the result
    return(result)
}