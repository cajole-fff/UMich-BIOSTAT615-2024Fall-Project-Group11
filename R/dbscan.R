library(R6)

#' @title dbscan
#'
#' @description A class for DBSCAN clustering.
#'
#' @details #TODO: fill in details w/ next few lines
#'
#' @author Fan Zhang
#' @created 2024-11-18 01:59:21
#'
#' @field field_name1 Description of the first field.
#' @field field_name2 Description of the second field.
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
DBSCAN615::dbscan <- R6::R6Class(
    "dbscan",
    public = list(
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
        },
        fit = function(
            X, # {array-like, sparse matrix} of shape (n_samples, n_features), or (n_samples, n_samples)
            y = NULL, # Ignored, present here for API consistency by convention
            sample_weight = NULL # array-like of shape (n_samples,)
        ) {
            #TODO: complete fit method

        },
        fit_predict = function(
            X, # {array-like, sparse matrix} of shape (n_samples, n_features), or (n_samples, n_samples)
            y = NULL, # Ignored, present here for API consistency by convention
            sample_weight = NULL # array-like of shape (n_samples,)
        ) {
            #TODO: complete fit_predict method
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

        ..core_sample_indices_ = NULL,
        ..components_ = NULL,
        ..labels_ = NULL,
        ..n_features_in_ = NULL,
        ..feature_names_in_ = NULL
    )
)