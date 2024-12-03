library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/metrics/metric_adjusted_rand_index.cpp"))

#' @title Adjusted Rand Index Metric
#' @description Computes the Adjusted Rand Index (ARI) for clustering evaluation.
#' @param true_labels An integer vector of true cluster labels.
#' @param pred_labels An integer vector of predicted cluster labels.
#' @return A numeric value representing the ARI.
metric_adjusted_rand_index <- function(
    true_labels, 
    pred_labels
) {
    return(metric_adjusted_rand_index_cpp(true_labels, pred_labels))
}