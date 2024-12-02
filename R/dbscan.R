options(here.show_startup_message = FALSE)
library(here)
library(R6)
source(here("R/BaseEstimator.R"))
source(here("R/utils/dbscan_fit.R"))
source(here("R/utils/error_handling.R"))

#' @title DBSCAN R6 Class
#'
#' @description A class for performing DBSCAN clustering.
#'
#' @details This class implements the DBSCAN clustering algorithm.
#' 
#' Private fields (for internal use only):
#' - **..eps**: float, default=0.5. The maximum distance between two samples for them to be considered as in the neighborhood of each other.
#' - **..min_samples**: int, default=5. The minimum number of samples required to form a cluster.
#' - **..metric**: str or callable, default="euclidean". The metric to use when calculating distance between instances in a feature array.
#' - **..metric_params**: list, default=NULL. Additional arguments for the metric function.
#' - **..algorithm**: str, default="auto". The algorithm used to compute nearest neighbors.
#' - **..leaf_size**: int, default=30. Leaf size passed to BallTree or KDTree.
#' - **..p**: int, default=NULL. Power parameter for the Minkowski metric.
#' - **..n_jobs**: int, default=NULL. The number of parallel jobs to run for neighbor search.
#'
#' @export
DBSCAN <- R6::R6Class(
    "DBSCAN",
    inherit = BaseEstimator,
    public = list(
        #' @description Initializes the DBSCAN class with specified parameters.
        #' @param eps float, default=0.5. The maximum distance between two samples for one to be considered in the same neighborhood.
        #' @param min_samples int, default=5. The minimum number of samples required to form a cluster.
        #' @param metric str or callable, default="euclidean". The distance metric to use.
        #' @param metric_params list, default=NULL. Additional arguments for the metric function.
        #' @param algorithm str, default="auto". The algorithm used to compute nearest neighbors.
        #' @param leaf_size int, default=30. Leaf size passed to BallTree or KDTree.
        #' @param p int, default=NULL. Power parameter for Minkowski metric. 1 is Manhattan, 2 is Euclidean, infinity is Chebyshev.
        #' @param n_jobs int, default=NULL. The number of parallel jobs to run.
        initialize = function(
            eps = 0.5, 
            min_samples = 5, 
            metric = "euclidean", 
            metric_params = NULL, 
            algorithm = "auto", 
            leaf_size = 30, 
            p = 2.0, 
            n_jobs = 1
        ) {
            if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) {
                stop(get_error_message(1001))
            }
            if (!is.numeric(min_samples) || length(min_samples) != 1 || min_samples <= 0 || floor(min_samples) != min_samples) {
                stop(get_error_message(1002))
            }
            if (!is.character(metric) || length(metric) != 1) {
                stop(get_error_message(1003))
            }
            if (!is.null(metric_params) && !is.list(metric_params)) {
                stop(get_error_message(1004))
            }
            if (!is.character(algorithm) || length(algorithm) != 1) {
                stop(get_error_message(1005))
            }
            if (!is.numeric(leaf_size) || length(leaf_size) != 1 || leaf_size <= 0 || floor(leaf_size) != leaf_size) {
                stop(get_error_message(1006))
            }
            if (!is.numeric(p) || length(p) != 1 || p <= 0) {
                stop(get_error_message(1007))
            }
            if (!is.numeric(n_jobs) || length(n_jobs) != 1 || floor(n_jobs) != n_jobs) {
                stop(get_error_message(1008))
            }
            private$..eps <- eps
            private$..min_samples <- min_samples
            private$..metric <- metric
            private$..metric_params <- metric_params
            private$..algorithm <- algorithm
            private$..leaf_size <- leaf_size
            private$..p <- p
            private$..n_jobs <- n_jobs
            private$..labels <- NULL
            private$..n_clusters <- NULL
            private$..core_sample_indices <- NULL
            private$..components <- NULL
            private$..n_features_in <- NULL
            private$..feature_names_in <- NULL
        },

        #' @description Fits the DBSCAN clustering model on the input data.
        #' @param X A matrix or data frame. The input data to be clustered.
        #' @param y Ignored, present for API consistency.
        #' @param sample_weight Numeric vector. Weights for each sample.
        #' @return The instance itself invisibly.
        fit = function(
            X, 
            y = NULL, 
            sample_weight = NULL
        ) {
            result <- dbscan_fit(
                X = as.matrix(X),
                eps = private$..eps,
                min_samples = private$..min_samples,
                metric = private$..metric,
                metric_params = private$..metric_params,
                algorithm = private$..algorithm,
                leaf_size = private$..leaf_size,
                p = private$..p,
                n_jobs = private$..n_jobs
            )
            private$..labels <- result$labels
            private$..n_clusters <- length(unique(result$labels[result$labels != -1]))
            private$..core_sample_indices <- result$core_sample_indices
            private$..components <- result$components
            private$..n_features_in <- ncol(X)
            private$..feature_names_in <- colnames(X)
            invisible(self)
        },

        #' @description Fits the DBSCAN clustering model on the input data and returns cluster labels.
        #' @param X A matrix or data frame. The input data to be clustered.
        #' @param y Ignored, present for API consistency.
        #' @param sample_weight Numeric vector. Weights for each sample.
        #' @return A numeric vector of cluster labels.
        fit_predict = function(
            X, 
            y = NULL, 
            sample_weight = NULL
        ) {
            self$fit(X, y, sample_weight)
            return(private$..labels)
        },

        #' @description Retrieves the cluster labels.
        #' @return A numeric vector of cluster labels.
        get_labels = function() {
            return(private$..labels)
        },

        #' @description Retrieves the number of clusters.
        #' @return An integer representing the number of clusters.
        get_n_clusters = function() {
            return(private$..n_clusters)
        },

        #' @description Retrieves the indices of core samples.
        #' @return A numeric vector of core sample indices.
        get_core_sample_indices = function() {
            return(private$..core_sample_indices)
        },

        #' @description Retrieves the components of core samples.
        #' @return A matrix of core sample components.
        get_components = function() {
            return(private$..components)
        },

        #' @description Retrieves the number of features in the input data.
        #' @return An integer representing the number of features.
        get_n_features_in = function() {
            return(private$..n_features_in)
        },

        #' @description Retrieves the feature names from the input data.
        #' @return A character vector of feature names.
        get_feature_names_in = function() {
            return(private$..feature_names_in)
        }
    ),

    private = list(
        ..eps = 0.5,
        ..min_samples = 5,
        ..metric = "euclidean",
        ..metric_params = NULL,
        ..algorithm = "auto",
        ..leaf_size = 30,
        ..p = NULL,
        ..n_jobs = NULL,

        ..labels = NULL,
        ..n_clusters = NULL,
        ..core_sample_indices = NULL,
        ..components = NULL,
        ..n_features_in = NULL,
        ..feature_names_in = NULL
    )
)
