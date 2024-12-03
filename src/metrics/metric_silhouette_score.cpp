#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// Helper function to calculate Euclidean distance
double euclidean_distance(const NumericVector &a, const NumericVector &b) {
    double dist = 0.0;
    for (R_xlen_t i = 0; i < a.size(); ++i) {
        dist += std::pow(a[i] - b[i], 2);
    }
    return std::sqrt(dist);
}

// [[Rcpp::export]]
double metric_silhouette_score_cpp(NumericMatrix X, IntegerVector labels) {
    int n = X.nrow();
    std::vector<double> sil_widths(n);
    
    for (int i = 0; i < n; ++i) {
        std::vector<double> same_cluster_dists;
        std::vector<double> other_cluster_dists;
        
        for (int j = 0; j < n; ++j) {
            if (i == j) continue;
            double dist = euclidean_distance(X.row(i), X.row(j));
            if (labels[i] == labels[j]) {
                same_cluster_dists.push_back(dist);
            } else {
                other_cluster_dists.push_back(dist);
            }
        }
        
        double a_i = (same_cluster_dists.empty() ? 0.0 : 
                      std::accumulate(same_cluster_dists.begin(), same_cluster_dists.end(), 0.0) / same_cluster_dists.size());
        double b_i = (other_cluster_dists.empty() ? std::numeric_limits<double>::max() : 
                      *std::min_element(other_cluster_dists.begin(), other_cluster_dists.end()));
        
        if (a_i == 0.0 && b_i == 0.0) {
            sil_widths[i] = 0.0;  // 如果同类和非同类距离都为 0，则轮廓系数为 0
        } else {
            sil_widths[i] = (b_i - a_i) / std::max(a_i, b_i);
        }
    }
    
    double mean_score = std::accumulate(sil_widths.begin(), sil_widths.end(), 0.0) / n;
    return mean_score;
}
