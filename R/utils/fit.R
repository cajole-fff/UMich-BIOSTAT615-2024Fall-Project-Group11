library(Rcpp)
Rcpp::sourceCpp("./src/fit.cpp")

#' @description Fits the DBSCAN clustering model on the input data using Rcpp.
#' @param X array-like or sparse matrix of shape (n_samples, n_features).
#' The input data to be clustered.
#' @return self Returns the instance itself.

fit <- function(
    X, # {array-like, sparse matrix} of shape (n_samples, n_features)
    y = NULL, # Ignored, present here for API consistency by convention
    sample_weight = NULL # array-like of shape (n_samples,)
) {
    # Validate input
    if (is.null(X)) stop("Input data X cannot be NULL.")
    if (!is.matrix(X)) stop("Input data X must be a matrix.")
    
    # Call the C++ DBSCAN implementation
    result <- dbscan_fit(
        data = as.matrix(X),
        eps = private$..eps,
        min_samples = private$..min_samples
    )
    
    # Store results in private fields
    private$..labels_ <- result$labels
    private$..n_clusters_ <- result$n_clusters
    private$..n_features_in_ <- ncol(X)
    private$..feature_names_in_ <- colnames(X)
    
    # Return self
    invisible(self)
}