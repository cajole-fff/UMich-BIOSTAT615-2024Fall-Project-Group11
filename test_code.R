library(Rcpp)
library(dbscan)
library(microbenchmark)
sourceCpp("D:/util_dbscan_fit.cpp")  

set.seed(123)
n_points <- 10000  
n_features <- 100   
data <- matrix(rnorm(n_points * n_features), nrow = n_points, ncol = n_features)
system.time({
  res_dbscan <- dbscan::dbscan(data, eps = 0.5, minPts = 5)
})
system.time({
  res_custom <- util_dbscan_fit_cpp(data, eps = 0.5, min_samples = 5, n_jobs = 4)
})
library(microbenchmark)

benchmark_results <- microbenchmark(
  dbscan_pkg = dbscan::dbscan(data, eps = 0.5, minPts = 5),
  custom_impl = util_dbscan_fit_cpp(data, eps = 0.5, min_samples = 5, n_jobs = 4),
  times = 10 
)

print(benchmark_results)
all.equal(res_custom$labels, res_dbscan$cluster)
library(pryr)

res_dbscan <- dbscan::dbscan(data, eps = 0.5, minPts = 5)
cat("dbscan package memory:", object_size(res_dbscan), "bytes\n")

res_custom <- util_dbscan_fit_cpp(data, eps = 0.5, min_samples = 5, n_jobs = 4)
cat("Custom implementation memory :", object_size(res_custom), "bytes\n")
