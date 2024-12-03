library(Rcpp)
library(here)
Rcpp::sourceCpp(here("src/metrics/metric_adjusted_rand_index.cpp"))

metric_adjusted_rand_index <- function(
    true_labels, 
    pred_labels
) {
    return(metric_adjusted_rand_index_cpp(true_labels, pred_labels))
}