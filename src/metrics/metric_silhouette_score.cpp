#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>
#include <unordered_map>

// Helper function to calculate Euclidean distance matrix
Rcpp::NumericMatrix compute_distance_matrix(const Rcpp::NumericMatrix &X) {
    int n = X.nrow();
    Rcpp::NumericMatrix dist_matrix(n, n);
    
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            double dist = 0.0;
            for (int k = 0; k < X.ncol(); ++k) {
                dist += std::pow(X(i, k) - X(j, k), 2);
            }
            dist = std::sqrt(dist);
            dist_matrix(i, j) = dist;
            dist_matrix(j, i) = dist;
        }
    }
    return dist_matrix;
}

// [[Rcpp::export]]
double metric_silhouette_score_cpp(Rcpp::NumericMatrix X, Rcpp::IntegerVector labels) {
    int n = X.nrow();
    Rcpp::NumericMatrix dist_matrix = compute_distance_matrix(X);
    std::vector<double> sil_widths(n);
    
    // Find unique cluster labels
    Rcpp::IntegerVector unique_labels = Rcpp::sort_unique(labels);
    int num_clusters = unique_labels.size();
    
    for (int i = 0; i < n; ++i) {
        // Compute a(i): Average distance to points in the same cluster
        double a_i = 0.0;
        int same_cluster_count = 0;
        
        for (int j = 0; j < n; ++j) {
            if (labels[i] == labels[j] && i != j) {
                a_i += dist_matrix(i, j);
                ++same_cluster_count;
            }
        }
        if (same_cluster_count > 0) {
            a_i /= same_cluster_count;
        }
        
        // Compute b(i): Minimum average distance to points in other clusters
        double b_i = std::numeric_limits<double>::max();
        for (int label : unique_labels) {
            if (label == labels[i]) continue;
            double b_i_temp = 0.0;
            int other_cluster_count = 0;
            for (int j = 0; j < n; ++j) {
                if (labels[j] == label) {
                    b_i_temp += dist_matrix(i, j);
                    ++other_cluster_count;
                }
            }
            if (other_cluster_count > 0) {
                b_i_temp /= other_cluster_count;
                if (b_i_temp < b_i) {
                    b_i = b_i_temp;
                }
            }
        }
        
        // Compute silhouette width for point i
        if (same_cluster_count > 0 && num_clusters > 1) {
            sil_widths[i] = (b_i - a_i) / std::max(a_i, b_i);
        } else {
            sil_widths[i] = 0.0;
        }
    }
    
    // Compute mean silhouette score
    double mean_score = std::accumulate(sil_widths.begin(), sil_widths.end(), 0.0) / n;
    return mean_score;
}