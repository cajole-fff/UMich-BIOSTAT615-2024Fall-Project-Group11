metric_noise_ratio <- function(labels) {
    return(sum(labels == -1) / length(labels))
}