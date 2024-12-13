#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <thread>
#include <future>
#include <mutex>
#include <algorithm>

#include "util_matrix.h"

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

inline double minkowski_distance_sparse(const SparseRow &a, const SparseRow &b, double p) {
    double dist = 0.0;
    size_t ia = 0, ib = 0;
    while (ia < a.size() && ib < b.size()) {
        if (a[ia].first == b[ib].first) {
            double diff = std::abs(a[ia].second - b[ib].second);
            dist += std::pow(diff, p);
            ia++; ib++;
        } else if (a[ia].first < b[ib].first) {
            double diff = std::abs(a[ia].second);
            dist += std::pow(diff, p);
            ia++;
        } else {
            double diff = std::abs(b[ib].second);
            dist += std::pow(diff, p);
            ib++;
        }
    }
    while (ia < a.size()) {
        double diff = std::abs(a[ia].second);
        dist += std::pow(diff, p);
        ia++;
    }
    while (ib < b.size()) {
        double diff = std::abs(b[ib].second);
        dist += std::pow(diff, p);
        ib++;
    }
    return std::pow(dist, 1.0 / p);
}

inline double minkowski_distance_dense(const NumericMatrix::ConstRow &a, const NumericMatrix::ConstRow &b, double p) {
    double dist = 0.0;
    for (R_xlen_t i = 0; i < a.size(); ++i) {
        dist += std::pow(std::abs(a[i] - b[i]), p);
    }
    return std::pow(dist, 1.0 / p);
}

NumericMatrix compute_distance_matrix_dense(const NumericMatrix &X, const std::string &metric, double p) {
    int n_samples = X.nrow();
    NumericMatrix dist_matrix(n_samples, n_samples);
    for (int i = 0; i < n_samples; ++i) {
        for (int j = i + 1; j < n_samples; ++j) {
            double dist;
            if (metric == "euclidean") {
                dist = minkowski_distance_dense(X.row(i), X.row(j), 2.0);
            } else if (metric == "minkowski") {
                dist = minkowski_distance_dense(X.row(i), X.row(j), p);
            } else {
                Rcpp::stop("Unsupported metric");
            }
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist;
        }
    }
    return dist_matrix;
}

NumericMatrix compute_distance_matrix_sparse(const std::vector<SparseRow> &rows, int n_samples, const std::string &metric, double p) {
    NumericMatrix dist_matrix(n_samples, n_samples);
    for (int i = 0; i < n_samples; ++i) {
        for (int j = i + 1; j < n_samples; ++j) {
            double dist;
            if (metric == "euclidean") {
                dist = minkowski_distance_sparse(rows[i], rows[j], 2.0);
            } else if (metric == "minkowski") {
                dist = minkowski_distance_sparse(rows[i], rows[j], p);
            } else {
                Rcpp::stop("Unsupported metric");
            }
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist;
        }
    }
    return dist_matrix;
}

// [[Rcpp::export]]
List util_dbscan_fit_cpp(SEXP X,
                         double eps,
                         int min_samples,
                         std::string metric = "euclidean",
                         List metric_params = R_NilValue,
                         std::string algorithm = "auto",
                         int leaf_size = 30,
                         double p = 2,
                         int n_jobs = 1) {
    // 判断X类型
    bool is_sparse = false;
    NumericMatrix X_dense;
    int n_samples = 0;
    int n_features = 0;

    std::vector<SparseRow> sparse_rows;

    if (Rf_inherits(X, "dgCMatrix")) {
        is_sparse = true;
        S4 mat(X);
        IntegerVector i = mat.slot("i");
        IntegerVector p_vec = mat.slot("p");
        NumericVector x = mat.slot("x");
        IntegerVector dim = mat.slot("Dim");
        n_samples = dim[0];
        n_features = dim[1];
        sparse_rows = dgCMatrix_to_row_structure(i, p_vec, x, n_samples, n_features);
    } else {
        X_dense = as<NumericMatrix>(X);
        n_samples = X_dense.nrow();
        n_features = X_dense.ncol();
    }

    NumericMatrix dist_matrix(n_samples, n_samples);
    if (is_sparse) {
        dist_matrix = compute_distance_matrix_sparse(sparse_rows, n_samples, metric, p);
    } else {
        dist_matrix = compute_distance_matrix_dense(X_dense, metric, p);
    }

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

    std::vector<bool> core_samples(n_samples, false);
    for (int i = 0; i < n_samples; ++i) {
        core_samples[i] = int(neighborhoods[i].size()) >= min_samples;
    }

    IntegerVector labels(n_samples, -1);

    int cluster_id = 0;
    std::vector<bool> visited(n_samples, false);

    for (int i = 0; i < n_samples; ++i) {
        if (visited[i]) continue;
        visited[i] = true;

        if (!core_samples[i]) continue;

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

    std::vector<int> core_sample_indices;
    for (int i = 0; i < n_samples; ++i) {
        if (core_samples[i]) {
            core_sample_indices.push_back(i);
        }
    }

    NumericMatrix components(core_sample_indices.size(), n_features);
    if (is_sparse) {
        for (size_t idx = 0; idx < core_sample_indices.size(); ++idx) {
            int r = core_sample_indices[idx];
            for (auto &kv : sparse_rows[r]) {
                components(idx, kv.first) = kv.second;
            }
        }
    } else {
        for (size_t i = 0; i < core_sample_indices.size(); ++i) {
            components(i, _) = X_dense.row(core_sample_indices[i]);
        }
    }

    return List::create(
        Named("labels") = labels,
        Named("core_sample_indices") = core_sample_indices,
        Named("components") = components,
        Named("n_features") = n_features
    );
}
