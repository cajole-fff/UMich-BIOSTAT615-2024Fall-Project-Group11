#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <unordered_map>

#include "util_matrix.h"

inline double euclidean_distance_sparse(const SparseRow &a, const SparseRow &b) {
    double dist = 0.0;
    size_t ia = 0, ib = 0;
    while (ia < a.size() && ib < b.size()) {
        if (a[ia].first == b[ib].first) {
            double diff = a[ia].second - b[ib].second;
            dist += diff * diff;
            ia++; ib++;
        } else if (a[ia].first < b[ib].first) {
            double diff = a[ia].second;
            dist += diff * diff;
            ia++;
        } else {
            double diff = b[ib].second;
            dist += diff * diff;
            ib++;
        }
    }
    while (ia < a.size()) {
        double diff = a[ia].second;
        dist += diff * diff;
        ia++;
    }
    while (ib < b.size()) {
        double diff = b[ib].second;
        dist += diff * diff;
        ib++;
    }
    return std::sqrt(dist);
}

inline double euclidean_distance_dense(const Rcpp::NumericMatrix::ConstRow &a, const Rcpp::NumericMatrix::ConstRow &b) {
    double dist = 0.0;
    for (int k = 0; k < a.size(); ++k) {
        double diff = a[k] - b[k];
        dist += diff * diff;
    }
    return std::sqrt(dist);
}

Rcpp::NumericMatrix compute_distance_matrix_dense(const Rcpp::NumericMatrix &X) {
    int n = X.nrow();
    Rcpp::NumericMatrix dist_matrix(n, n);
    
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            double dist = euclidean_distance_dense(X.row(i), X.row(j));
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist;
        }
    }
    return dist_matrix;
}

Rcpp::NumericMatrix compute_distance_matrix_sparse(const std::vector<SparseRow> &rows) {
    int n = (int) rows.size();
    Rcpp::NumericMatrix dist_matrix(n, n);
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            double dist = euclidean_distance_sparse(rows[i], rows[j]);
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist;
        }
    }
    return dist_matrix;
}

// [[Rcpp::export]]
double metric_silhouette_score_cpp(Rcpp::RObject X, Rcpp::IntegerVector labels) {
    int n;
    Rcpp::NumericMatrix dist_matrix;
    std::vector<SparseRow> sparse_rows;
    int ncol;

    if (Rcpp::is<Rcpp::NumericMatrix>(X)) {
        Rcpp::NumericMatrix X_dense(X);
        n = X_dense.nrow();
        dist_matrix = compute_distance_matrix_dense(X_dense);
        ncol = X_dense.ncol();
    } else if (Rf_inherits(X, "dgCMatrix")) {
        Rcpp::S4 mat(X);
        Rcpp::IntegerVector i = mat.slot("i");
        Rcpp::IntegerVector p = mat.slot("p");
        Rcpp::NumericVector x = mat.slot("x");
        Rcpp::IntegerVector dim = mat.slot("Dim");
        n = dim[0];
        ncol = dim[1];

        sparse_rows = dgCMatrix_to_row_structure(i, p, x, n, ncol);
        dist_matrix = compute_distance_matrix_sparse(sparse_rows);
    } else {
        Rcpp::stop("X must be either a numeric matrix or a dgCMatrix.");
    }

    std::vector<double> sil_widths(n);

    Rcpp::IntegerVector unique_labels = Rcpp::sort_unique(labels);
    int num_clusters = unique_labels.size();

    for (int idx = 0; idx < n; ++idx) {
        double a_i = 0.0;
        int same_cluster_count = 0;
        int cluster_label = labels[idx];

        for (int j = 0; j < n; ++j) {
            if (labels[j] == cluster_label && j != idx) {
                a_i += dist_matrix(idx, j);
                same_cluster_count++;
            }
        }
        if (same_cluster_count > 0) {
            a_i /= same_cluster_count;
        }

        double b_i = std::numeric_limits<double>::max();

        for (int cl : unique_labels) {
            if (cl == cluster_label) continue;
            double cluster_dist = 0.0;
            int cluster_count = 0;
            for (int j = 0; j < n; ++j) {
                if (labels[j] == cl) {
                    cluster_dist += dist_matrix(idx, j);
                    cluster_count++;
                }
            }
            if (cluster_count > 0) {
                double avg_dist = cluster_dist / cluster_count;
                if (avg_dist < b_i) {
                    b_i = avg_dist;
                }
            }
        }

        if (same_cluster_count > 0 && num_clusters > 1) {
            sil_widths[idx] = (b_i - a_i) / std::max(a_i, b_i);
        } else {
            sil_widths[idx] = 0.0;
        }
    }

    double mean_score = std::accumulate(sil_widths.begin(), sil_widths.end(), 0.0) / n;
    return mean_score;
}
