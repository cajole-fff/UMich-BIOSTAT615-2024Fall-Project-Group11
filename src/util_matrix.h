#ifndef MATRIX_UTILS_H
#define MATRIX_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <utility>

typedef std::vector<std::pair<int, double>> SparseRow;

std::vector<SparseRow> dgCMatrix_to_row_structure(const Rcpp::IntegerVector &i,
                                                  const Rcpp::IntegerVector &p,
                                                  const Rcpp::NumericVector &x,
                                                  int nrow, int ncol);

#endif // MATRIX_UTILS_H
