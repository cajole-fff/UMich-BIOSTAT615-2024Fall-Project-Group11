#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <queue>

using namespace Rcpp;

// Helper function to calculate Euclidean distance between two points
double euclidean_distance(const NumericVector &a, const NumericVector &b) {
    double dist = 0.0;
    for (R_xlen_t i = 0; i < a.size(); ++i) { // 修改循环变量类型为 R_xlen_t
        dist += std::pow(a[i] - b[i], 2);
    }
    return std::sqrt(dist);
}

// [[Rcpp::export]]
List dbscan_fit_cpp(NumericMatrix data, double eps, int min_samples) {
    R_xlen_t n_samples = data.nrow();
    NumericVector labels(n_samples, -1); // Initialize labels with -1 (noise)
    std::vector<bool> visited(n_samples, false); // Track visited points
    int cluster_id = 0; // Cluster counter

    // Helper function to find neighbors within eps distance
    auto get_neighbors = [&](R_xlen_t idx) {
        std::vector<R_xlen_t> neighbors;
        for (R_xlen_t i = 0; i < n_samples; ++i) {
            if (euclidean_distance(data(idx, _), data(i, _)) <= eps) {
                neighbors.push_back(i);
            }
        }
        return neighbors;
    };

    // Expand cluster function
    auto expand_cluster = [&](R_xlen_t point_idx, std::vector<R_xlen_t> &neighbors) {
        labels[point_idx] = cluster_id;
        std::queue<R_xlen_t> neighbor_queue;
        for (R_xlen_t n : neighbors) neighbor_queue.push(n);

        while (!neighbor_queue.empty()) {
            R_xlen_t curr_idx = neighbor_queue.front();
            neighbor_queue.pop();

            if (!visited[curr_idx]) {
                visited[curr_idx] = true;
                std::vector<R_xlen_t> curr_neighbors = get_neighbors(curr_idx);
                if ((int)curr_neighbors.size() >= min_samples) {
                    for (R_xlen_t n : curr_neighbors) {
                        neighbor_queue.push(n);
                    }
                }
            }
            if (labels[curr_idx] == -1) {
                labels[curr_idx] = cluster_id;
            }
        }
    };

    // Main loop
    for (R_xlen_t i = 0; i < n_samples; ++i) {
        if (!visited[i]) {
            visited[i] = true;
            std::vector<R_xlen_t> neighbors = get_neighbors(i);
            if ((int)neighbors.size() < min_samples) {
                labels[i] = -1; // Mark as noise
            } else {
                cluster_id++;
                expand_cluster(i, neighbors);
            }
        }
    }

    // Return labels and cluster information
    return List::create(
        _["labels"] = labels,
        _["n_clusters"] = cluster_id
    );
}
