#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <thread>
#include <future>
#include <mutex>

using namespace Rcpp;

// Helper function to calculate Minkowski distance
inline double minkowski_distance(const NumericVector &a, const NumericVector &b, double p) {
    double dist = 0.0;
    for (R_xlen_t i = 0; i < a.size(); ++i) {
        dist += std::pow(std::abs(a[i] - b[i]), p);
    }
    return std::pow(dist, 1.0 / p);
}

// Precompute distance matrix
NumericMatrix compute_distance_matrix(const NumericMatrix &X, const std::string &metric, double p) {
    int n_samples = X.nrow();
    NumericMatrix dist_matrix(n_samples, n_samples);

    for (int i = 0; i < n_samples; ++i) {
        for (int j = i + 1; j < n_samples; ++j) {
            double dist;
            if (metric == "euclidean") {
                dist = minkowski_distance(X.row(i), X.row(j), 2.0);
            } else if (metric == "minkowski") {
                dist = minkowski_distance(X.row(i), X.row(j), p);
            } else {
                Rcpp::stop("Unsupported metric");
            }
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist; // Symmetric
        }
    }
    return dist_matrix;
}

// Compute neighborhoods from distance matrix
std::vector<std::vector<int>> compute_neighborhoods(const NumericMatrix &dist_matrix, double eps) {
    int n_samples = dist_matrix.nrow();
    std::vector<std::vector<int>> neighborhoods(n_samples);

    for (int i = 0; i < n_samples; ++i) {
        for (int j = 0; j < n_samples; ++j) {
            if (dist_matrix(i, j) <= eps) {
                neighborhoods[i].push_back(j);
            }
        }
    }
    return neighborhoods;
}

// [[Rcpp::export]]
List util_dbscan_fit_cpp(NumericMatrix X,
                         double eps,
                         int min_samples,
                         std::string metric = "euclidean",
                         List metric_params = R_NilValue,
                         std::string algorithm = "auto",
                         int leaf_size = 30,
                         double p = 2,
                         int n_jobs = 1) {
    int n_samples = X.nrow();
    int n_features = X.ncol();

    // Precompute distance matrix
    NumericMatrix dist_matrix = compute_distance_matrix(X, metric, p);

    // Compute neighborhoods (parallelized)
    std::vector<std::vector<int>> neighborhoods(n_samples);
    std::mutex mtx;

    auto compute_task = [&](int start_idx, int end_idx) {
        for (int i = start_idx; i < end_idx; ++i) {
            std::vector<int> neighborhood;
            for (int j = 0; j < n_samples; ++j) {
                if (dist_matrix(i, j) <= eps) {
                    neighborhood.push_back(j);
                }
            }
            std::lock_guard<std::mutex> lock(mtx);
            neighborhoods[i] = std::move(neighborhood);
        }
    };

    if (n_jobs > 1) {
        std::vector<std::thread> threads;
        int chunk_size = (n_samples + n_jobs - 1) / n_jobs;

        for (int i = 0; i < n_jobs; ++i) {
            int start_idx = i * chunk_size;
            int end_idx = std::min(start_idx + chunk_size, n_samples);
            threads.emplace_back(compute_task, start_idx, end_idx);
        }

        for (auto &thread : threads) {
            thread.join();
        }
    } else {
        compute_task(0, n_samples);
    }

    // Determine core samples
    std::vector<bool> core_samples(n_samples, false);
    for (int i = 0; i < n_samples; ++i) {
        core_samples[i] = neighborhoods[i].size() >= min_samples;
    }

    // Initialize labels, -1 means noise
    IntegerVector labels(n_samples, -1);

    int cluster_id = 0;
    std::vector<bool> visited(n_samples, false);

    // Main loop: process all samples
    for (int i = 0; i < n_samples; ++i) {
        if (visited[i]) continue;
        visited[i] = true;

        if (!core_samples[i]) continue; // Skip non-core points

        // Start a new cluster
        std::vector<int> cluster_queue = neighborhoods[i];
        labels[i] = cluster_id;

        while (!cluster_queue.empty()) {
            int current_point = cluster_queue.back();
            cluster_queue.pop_back();

            if (!visited[current_point]) {
                visited[current_point] = true;

                if (core_samples[current_point]) {
                    cluster_queue.insert(cluster_queue.end(),
                                         neighborhoods[current_point].begin(),
                                         neighborhoods[current_point].end());
                }
            }

            if (labels[current_point] == -1) {
                labels[current_point] = cluster_id;
            }
        }

        cluster_id++;
    }

    // Collect core sample indices
    std::vector<int> core_sample_indices;
    for (int i = 0; i < n_samples; ++i) {
        if (core_samples[i]) {
            core_sample_indices.push_back(i);
        }
    }

    // Prepare core sample data
    NumericMatrix components(core_sample_indices.size(), n_features);
    for (size_t i = 0; i < core_sample_indices.size(); ++i) {
        components(i, _) = X.row(core_sample_indices[i]);
    }

    // Return result list
    return List::create(Named("labels") = labels,
                        Named("core_sample_indices") = core_sample_indices,
                        Named("components") = components,
                        Named("n_features") = n_features);
}
