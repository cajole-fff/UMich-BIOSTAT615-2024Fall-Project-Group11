library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/metrics/metric_silhouette_score.cpp"))

metric_silhouette_score <- function(
    X, 
    labels
) {
    return(metric_silhouette_score_cpp(X, labels))
}