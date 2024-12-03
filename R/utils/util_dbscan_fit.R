library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/utils/util_dbscan_fit.cpp"))
source(here("R/utils/util_error_handling.R"))

#' @title DBSCAN Clustering Fit Function
#' @description Fits the DBSCAN clustering model on the input data using Rcpp for efficient computation.
#' @param X matrix, a numeric matrix of shape (n_samples, n_features), representing the input data to be clustered.
#' @param eps numeric, the maximum distance between two samples for one to be considered as in the neighborhood of the other.
#' @param min_samples integer, the minimum number of samples in a neighborhood for a point to be considered a core point. This includes the point itself.
#' @param metric character, the metric to use when calculating distance between instances in a feature array. Supported values are `"euclidean"` or `"minkowski"`.
#' @param metric_params list, additional arguments for the metric function (default: `NULL`).
#' @param algorithm character, the algorithm used to compute nearest neighbors. Default is `"auto"`.
#' @param leaf_size integer, leaf size passed to BallTree or KDTree. Affects the speed of nearest neighbor queries. Default is `30`.
#' @param p numeric, power parameter for the Minkowski metric. When `p=1`, this is equivalent to the Manhattan distance. When `p=2`, this is equivalent to the Euclidean distance. Default is `2`.
#' @param n_jobs integer, the number of parallel jobs to run for neighborhood computation. If `1`, no parallelization is used. Default is `1`.
util_dbscan_fit <- function(
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
    if (is.null(X)) stop("Input data X cannot be NULL.", call. = FALSE, error_code = 2001)
    if (!is.matrix(X)) stop("Input data X must be a matrix.", call. = FALSE, error_code = 2002)

    # Try to call the C++ implementation
    result <- tryCatch({
        dbscan_fit_cpp(
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
    }, error = function(e) {
        stop(
            paste(
                get_error_message(3001),
                e$message
            ),
            call. = FALSE,
            error_code = 3001
        )
    })

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
