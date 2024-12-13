#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <thread>
#include <future>
#include <mutex>
#include <algorithm>

// [[Rcpp::plugins(cpp11)]]
#include "util_matrix.h"

using namespace Rcpp;

std::vector<SparseRow> dgCMatrix_to_row_structure(const IntegerVector &i, const IntegerVector &p, const NumericVector &x, int nrow, int ncol) {
    std::vector<SparseRow> rows(nrow);

    for (int col = 0; col < ncol; ++col) {
        for (int idx = p[col]; idx < p[col+1]; ++idx) {
            int row = i[idx];
            double val = x[idx];
            rows[row].push_back(std::make_pair(col, val));
        }
    }
    for (auto &r : rows) {
        std::sort(r.begin(), r.end(), [](const std::pair<int,double> &a, const std::pair<int,double> &b){
            return a.first < b.first;
        });
    }
    return rows;
}