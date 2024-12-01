options(here.show_startup_message = FALSE)
library(here)
library(R6)
source(here("R/BaseEstimator.R"))
source(here("R/utils/dbscan_fit.R"))

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
        #' @param p int, default=NULL. Power parameter for Minkowski metric.
        #' @param n_jobs int, default=NULL. The number of parallel jobs to run.
        initialize = function(
            eps = 0.5, 
            min_samples = 5, 
            metric = "euclidean", 
            metric_params = NULL, 
            algorithm = "auto", 
            leaf_size = 30, 
            p = NULL, 
            n_jobs = NULL
        ) {
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
        },

        #' @description Fits the DBSCAN clustering model on the input data.
        #' @param X A matrix or data frame. The input data to be clustered.
        #' @param y Ignored, present for API consistency.
        #' @param sample_weight Numeric vector. Weights for each sample.
        #' @return The instance itself invisibly.
        fit = function(X, y = NULL, sample_weight = NULL) {
            result <- dbscan_fit(
                X = X,
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
            private$..n_clusters <- result$n_clusters
            return(invisible(self))
        },

        #' @description Fits the DBSCAN clustering model on the input data and returns cluster labels.
        #' @param X A matrix or data frame. The input data to be clustered.
        #' @param y Ignored, present for API consistency.
        #' @param sample_weight Numeric vector. Weights for each sample.
        #' @return A numeric vector of cluster labels.
        fit_predict = function(X, y = NULL, sample_weight = NULL) {
            self$fit(X, y, sample_weight)
            return(private$..labels)
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
        ..n_clusters = NULL
    )
)
