library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/metrics/metric_silhouette_score.cpp"))

metric_silhouette_score <- function(
    X, 
    labels
) {
    score <- silhouette(labels, dist(as.matrix(X)))
    mean_score <- mean(score[, "sil_width"])
    return(mean_score)
}