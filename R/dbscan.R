library(R6)

#' @title dbscan
#'
#' @description A class for DBSCAN clustering.
#'
#' @details #TODO: fill in details w/ next few lines
#'
#' @field ..eps float, default=0.5. The maximum distance between two samples for one to be considered as in the neighborhood of the other.
#' @field ..min_samples int, default=5. The number of samples (or total weight) in a neighborhood for a point to be considered as a core point.
#' @field ..metric str or callable, default="euclidean". The metric to use when calculating distance between instances in a feature array.
#' @field ..metric_params list, default=NULL. Additional keyword arguments for the metric function.
#' @field ..algorithm str, default="auto". The algorithm to be used by the NearestNeighbors module to compute pointwise distances and find nearest neighbors.
#' @field ..leaf_size int, default=30. Leaf size passed to BallTree or KDTree.
#' @field ..p int, default=NULL. Power parameter for the Minkowski metric.
#' @field ..n_jobs int, default=NULL. The number of parallel jobs to run for neighbors search. If -1, then the number of jobs is set to the number of CPU cores.
#'
#' @section Methods:
#' \describe{
#'     \item{\code{initialize(args)}}{Description of the constructor method.}
#'     \item{\code{$method1()}}{Description of method1.}
#'     \item{\code{$method2(args)}}{Description of method2.}
#' }
#'
#' @section Private Methods:
#' \describe{
#'     \item{\code{private_method1()}}{Description of private_method1.}
#' }
#'
#' @examples
#' \dontrun{
#'     obj <- MyClassnew(arg1 = "value", arg2 = 42)
#'     obj$method1()
#'     result <- obj$method2(args)("example", 10)
#'     print(result)
#' }
#'
#' @export
DBSCAN <- R6::R6Class(
    "DBSCAN",
    inherit = BaseEstimator,
    public = list(
        #' @description Initializes the DBSCAN class with specified parameters.
        #' @param eps float, default=0.5
        #' The maximum distance between two samples for one to be considered as in the neighborhood of the other.
        #' @param min_samples int, default=5
        #' The number of samples (or total weight) in a neighborhood for a point to be considered as a core point.
        #' @param metric str or callable, default="euclidean"
        #'  The metric to use when calculating distance between instances in a feature array.
        #' @param metric_params list, default=NULL
        #' Additional keyword arguments for the metric function.
        #' @param algorithm str, default="auto"
        #' The algorithm to be used by the NearestNeighbors module to compute pointwise distances and find nearest neighbors.
        #' @param leaf_size int, default=30
        #' Leaf size passed to BallTree or KDTree.
        #' @param p int, default=NULL
        #' Power parameter for the Minkowski metric.
        #' @param n_jobs int, default=NULL
        #' The number of parallel jobs to run for neighbors search. If -1, then the number of jobs is set to the number of CPU cores.
        #' @return What the function returns.
        initialize = function(
            eps = 0.5, # float
            min_samples = 5, # int
            metric = "euclidean", # str or callable
            metric_params = NULL, # list
            algorithm = "auto", # str, {"auto", "ball_tree", "kd_tree", "brute"}
            leaf_size = 30, # int
            p = NULL, # int
            n_jobs = NULL # int
        ) {
            #TODO: input validation
            private$..eps <- eps
            private$..min_samples <- min_samples
            private$..metric <- metric
            private$..metric_params <- metric_params
            private$..algorithm <- algorithm
            private$..leaf_size <- leaf_size
            private$..p <- p
            private$..n_jobs <- n_jobs
        }
        # fit = function(
        #     X, # {array-like, sparse matrix} of shape (n_samples, n_features), or (n_samples, n_samples)
        #     y = NULL, # Ignored, present here for API consistency by convention
        #     sample_weight = NULL # array-like of shape (n_samples,)
        # ) {
        #     #TODO: complete fit method

        # },
        # fit_predict = function(
        #     X, # {array-like, sparse matrix} of shape (n_samples, n_features), or (n_samples, n_samples)
        #     y = NULL, # Ignored, present here for API consistency by convention
        #     sample_weight = NULL # array-like of shape (n_samples,)
        # ) {
        #     #TODO: complete fit_predict method
        # }
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

        ..core_sample_indices_ = NULL,
        ..components_ = NULL,
        ..labels_ = NULL,
        ..n_features_in_ = NULL,
        ..feature_names_in_ = NULL
    )
)