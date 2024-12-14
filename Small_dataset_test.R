library(Rcpp)
library(ggplot2)
library(microbenchmark)

sourceCpp("D:/util_dbscan_fit.cpp")  # Update path to actual location
sourceCpp("D:/metric_adjusted_rand_index.cpp")
sourceCpp("D:/metric_silhouette_score.cpp")

source("D:/util_dbscan_fit.R")
source("D:/metric_adjusted_rand_index.R")
source("D:/metric_silhouette_score.R")
source("D:/metric_noise_ratio.R")
source("D:/visu_plot_clusters.R")
source("D:/visu_plot_core_samples.R")

generate_test_data <- function() {
  set.seed(123)
  cluster1 <- MASS::mvrnorm(100, mu = c(0, 0), Sigma = diag(2))
  cluster2 <- MASS::mvrnorm(100, mu = c(10, 10), Sigma = diag(2))
  cluster3 <- MASS::mvrnorm(100, mu = c(0, 10), Sigma = diag(2))
  data <- rbind(cluster1, cluster2, cluster3)
  ground_truth <- rep(1:3, each = 100)
  list(data = data, ground_truth = ground_truth)
}


test_dbscan <- function() {
  test_data <- generate_test_data()
  data <- test_data$data
  ground_truth <- test_data$ground_truth
  eps <- 5
  min_samples <- 5
  clustering_result <- util_dbscan_fit(data, eps, min_samples)
  labels <- clustering_result$labels
  core_sample_indices <- clustering_result$core_sample_indices
  ari <- metric_adjusted_rand_index(ground_truth, labels)
  silhouette_score <- metric_silhouette_score(data, labels)
  noise_ratio <- metric_noise_ratio(labels)
  benchmark_results <- microbenchmark(
    dbscan_cpp = util_dbscan_fit(data, eps, min_samples, metric = "euclidean", n_jobs = 4),
    times = 10
  )
  

  list(
    ARI = ari,
    Silhouette_Score = silhouette_score,
    Noise_Ratio = noise_ratio,
    Benchmark = benchmark_results
  )
}

# Run the test
results <- test_dbscan()

cat("Adjusted Rand Index (ARI):", results$ARI, "\n")
cat("Silhouette Score:", results$Silhouette_Score, "\n")
cat("Noise Ratio:", results$Noise_Ratio, "\n")
print(results$Benchmark)

