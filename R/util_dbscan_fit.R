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
    X,
    eps,
    min_samples,
    metric = "euclidean",
    metric_params = NULL,
    algorithm = "auto",
    leaf_size = 30,
    p = 2,
    n_jobs = 1
) {
    if (is.null(X)) stop("Input data X cannot be NULL.", call. = FALSE)
    if (!inherits(X, "matrix") && !inherits(X, "dgCMatrix")) {
        stop("X must be either a regular matrix or a dgCMatrix.", call. = FALSE)
    }

    # 调用C++函数时传递X原样，如果是dgCMatrix的话，就会在C++那边解析
    result <- tryCatch({
        util_dbscan_fit_cpp(
            X = X,
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
                "An error occurred while fitting the DBSCAN model:",
                e$message
            ),
            call. = FALSE
        )
    })

    parsed_result <- list(
        labels = result$labels,
        core_sample_indices = result$core_sample_indices,
        components = result$components,
        n_features = result$n_features
    )

    return(parsed_result)
}
