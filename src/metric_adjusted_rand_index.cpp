#include <Rcpp.h>
#include <unordered_map>
#include <cmath>
#include <utility>

namespace std {
    template<>
    struct hash<std::pair<int, int>> {
        std::size_t operator()(const std::pair<int, int>& pair) const {
            return std::hash<int>()(pair.first) ^ std::hash<int>()(pair.second);
        }
    };
}

// Helper function to compute combinations nC2
double combination(int n) {
    return (n < 2) ? 0.0 : static_cast<double>(n * (n - 1)) / 2.0;
}

// [[Rcpp::export]]
double metric_adjusted_rand_index_cpp(Rcpp::IntegerVector true_labels, Rcpp::IntegerVector pred_labels) {
    int n = true_labels.size();
    if (n != pred_labels.size()) {
        Rcpp::stop("true_labels and pred_labels must have the same length.");
    }

    // Contingency matrix stored as a map of pairs
    std::unordered_map<std::pair<int, int>, int> contingency;
    std::unordered_map<int, int> sum_true;
    std::unordered_map<int, int> sum_pred;

    // Fill contingency table
    for (int i = 0; i < n; ++i) {
        std::pair<int, int> label_pair = {true_labels[i], pred_labels[i]};
        contingency[label_pair]++;
        sum_true[true_labels[i]]++;
        sum_pred[pred_labels[i]]++;
    }

    // Calculate index, expected index, and max index
    double index = 0.0, expected_index = 0.0, total_pairs = combination(n);
    double sum_true_pairs = 0.0, sum_pred_pairs = 0.0;

    for (const auto &entry : contingency) {
        index += combination(entry.second);
    }
    for (const auto &entry : sum_true) {
        sum_true_pairs += combination(entry.second);
    }
    for (const auto &entry : sum_pred) {
        sum_pred_pairs += combination(entry.second);
    }

    expected_index = (sum_true_pairs * sum_pred_pairs) / total_pairs;
    double max_index = 0.5 * (sum_true_pairs + sum_pred_pairs);

    // Compute Adjusted Rand Index
    return (index - expected_index) / (max_index - expected_index);
}
