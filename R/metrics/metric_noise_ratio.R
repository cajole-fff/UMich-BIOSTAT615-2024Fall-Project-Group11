#' @title Noise ratio
#' @description Computes the noise ratio for clustering evaluation.
#' @param labels An integer vector of cluster labels.
#' @return A numeric value representing the noise ratio.
metric_noise_ratio <- function(labels) {
    return(sum(labels == -1) / length(labels))
}