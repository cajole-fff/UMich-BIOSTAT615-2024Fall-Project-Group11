library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/dbscan_fit.cpp"))

#' @title DBSCAN Clustering Fit Function
#'
#' @description Fits the DBSCAN clustering model on the input data using Rcpp for efficient computation.
#'
#' @param X matrix 
#' A numeric matrix of shape (n_samples, n_features), representing the input data to be clustered.
#'
#' @param eps numeric 
#' The maximum distance between two samples for them to be considered as in the same neighborhood.
#'
#' @param min_samples integer 
#' The minimum number of samples in a neighborhood for a point to be considered a core point. This includes the point itself.
#'
#' @param metric character 
#' The metric to use when calculating distance between instances in a feature array. 
#' Supported values are `"euclidean"` or `"minkowski"`.
#'
#' @param metric_params list 
#' Additional arguments for the metric function (default: `NULL`).
#'
#' @param algorithm character 
#' The algorithm used to compute nearest neighbors. Default is `"auto"`.
#'
#' @param leaf_size integer 
#' Leaf size passed to BallTree or KDTree. Affects the speed of nearest neighbor queries. Default is `30`.
#'
#' @param p numeric 
#' Power parameter for the Minkowski metric. When `p=1`, this is equivalent to the Manhattan distance. 
#' When `p=2`, this is equivalent to the Euclidean distance. Default is `2`.
#'
#' @param n_jobs integer 
#' The number of parallel jobs to run for neighborhood computation. If `1`, no parallelization is used. Default is `1`.
#'
#' @details 
#' This function serves as an interface to the Rcpp implementation of DBSCAN clustering. It computes the clusters, 
#' identifies core samples, and returns key clustering results. The function relies on efficient computation 
#' for handling large datasets.
#'
#' @return 
#' A list containing the following elements:
#' \item{labels}{An integer vector of cluster labels for each sample in the input data. A label of `-1` indicates noise.}
#' \item{core_sample_indices}{An integer vector of indices representing the core samples.}
#' \item{components}{A numeric matrix of core samples, where each row corresponds to a core sample.}
#' \item{n_features}{An integer representing the number of features (columns) in the input data.}
#'
#' @examples
#' \dontrun{
#' # Example data
#' X <- matrix(runif(100), nrow = 20, ncol = 5)
#'
#' # Run DBSCAN
#' result <- dbscan_fit(
#'   X = X, 
#'   eps = 0.5, 
#'   min_samples = 5, 
#'   metric = "euclidean", 
#'   metric_params = NULL, 
#'   algorithm = "auto", 
#'   leaf_size = 30, 
#'   p = 2, 
#'   n_jobs = 1
#' )
#'
#' # Access results
#' print(result$labels)
#' print(result$core_sample_indices)
#' print(result$components)
#' }
#'
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

    # Parse the result into a structured list
    parsed_result <- list(
        labels = result$labels,
        core_sample_indices = result$core_sample_indices,
        components = result$components,
        n_features = result$n_features
    )

    # Return the parsed result
    return(parsed_result)
}