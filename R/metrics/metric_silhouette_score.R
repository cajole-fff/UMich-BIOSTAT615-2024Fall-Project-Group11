library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/metrics/metric_silhouette_score.cpp"))

#' @title Silhouette Score Metric
#' @description Computes the Silhouette Score for clustering evaluation.
#' @param X matrix, a numeric matrix of shape (n_samples, n_features), representing the input data to be clustered.
#' @param labels An integer vector of cluster labels.
#' @return A numeric value representing the Silhouette Score.
metric_silhouette_score <- function(
    X, 
    labels
) {
    return(metric_silhouette_score_cpp(X, labels))
}