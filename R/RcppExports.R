# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

metric_adjusted_rand_index_cpp <- function(true_labels, pred_labels) {
    .Call(`_DBSCAN615_metric_adjusted_rand_index_cpp`, true_labels, pred_labels)
}

metric_silhouette_score_cpp <- function(X, labels) {
    .Call(`_DBSCAN615_metric_silhouette_score_cpp`, X, labels)
}

util_dbscan_fit_cpp <- function(X, eps, min_samples, metric = "euclidean", metric_params = NULL, algorithm = "auto", leaf_size = 30L, p = 2, n_jobs = 1L) {
    .Call(`_DBSCAN615_util_dbscan_fit_cpp`, X, eps, min_samples, metric, metric_params, algorithm, leaf_size, p, n_jobs)
}

