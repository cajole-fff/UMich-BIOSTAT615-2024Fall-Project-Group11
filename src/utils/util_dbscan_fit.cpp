#include <Rcpp.h>
#include <vector>
#include <queue>
#include <cmath>
#include <thread>
#include <mutex>
#include <future>

using namespace Rcpp;
using namespace std;

// Helper function to calculate Minkowski distance
double minkowski_distance(const NumericVector &a, const NumericVector &b, double p) {
    double dist = 0.0;
    for (R_xlen_t i = 0; i < a.size(); ++i) {
        dist += std::pow(std::abs(a[i] - b[i]), p);
    }
    return std::pow(dist, 1.0 / p);
}

// Compute neighborhood for a given point
vector<int> compute_neighborhood(const NumericMatrix &X, int point_idx, double eps, const string &metric, double p) {
    int n_samples = X.nrow();
    NumericVector xi = X.row(point_idx);
    vector<int> neighborhood;

    for (int j = 0; j < n_samples; ++j) {
        NumericVector xj = X.row(j);

        double dist;
        if (metric == "euclidean") {
            dist = minkowski_distance(xi, xj, 2.0);
        } else if (metric == "minkowski") {
            dist = minkowski_distance(xi, xj, p);
        } else {
            Rcpp::stop("Unsupported metric");
        }

        if (dist <= eps) {
            neighborhood.push_back(j);
        }
    }
    return neighborhood;
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

    // Store each sample's neighborhood
    vector<vector<int>> neighborhoods(n_samples);
    
    // Mutex for thread-safe access to neighborhoods
    std::mutex mtx;

    // Compute neighborhoods in parallel if n_jobs > 1
    auto compute_task = [&](int start_idx, int end_idx) {
        for (int i = start_idx; i < end_idx; ++i) {
            vector<int> neighborhood = compute_neighborhood(X, i, eps, metric, p);
            std::lock_guard<std::mutex> lock(mtx);
            neighborhoods[i] = neighborhood;
        }
    };

    if (n_jobs > 1) {
        vector<std::thread> threads;
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
    vector<bool> core_samples(n_samples);
    for (int i = 0; i < n_samples; ++i) {
        core_samples[i] = int(neighborhoods[i].size()) >= min_samples;
    }

    // Initialize labels, -1 means noise
    IntegerVector labels(n_samples, -1);

    int cluster_id = 0;
    vector<bool> visited(n_samples, false);

    // Main loop: process all samples
    for (int i = 0; i < n_samples; ++i) {
        if (visited[i]) continue;
        visited[i] = true;

        if (!core_samples[i]) continue; // Skip non-core points

        // Start a new cluster
        queue<int> neighbors_queue;
        labels[i] = cluster_id;
        neighbors_queue.push(i);

        while (!neighbors_queue.empty()) {
            int current_point = neighbors_queue.front();
            neighbors_queue.pop();

            vector<int> current_neighbors = neighborhoods[current_point];

            for (int neighbor_point : current_neighbors) {
                if (!visited[neighbor_point]) {
                    visited[neighbor_point] = true;
                    if (core_samples[neighbor_point]) {
                        neighbors_queue.push(neighbor_point);
                    }
                }

                if (labels[neighbor_point] == -1) {
                    labels[neighbor_point] = cluster_id;
                }
            }
        }

        cluster_id++;
    }

    // Collect core sample indices
    vector<int> core_sample_indices;
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
