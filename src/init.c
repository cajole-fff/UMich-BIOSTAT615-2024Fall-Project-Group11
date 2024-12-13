#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

double metric_adjusted_rand_index_cpp(Rcpp::IntegerVector true_labels, Rcpp::IntegerVector pred_labels);
double metric_silhouette_score_cpp(Rcpp::NumericMatrix X, Rcpp::IntegerVector labels);
List util_dbscan_fit_cpp(NumericMatrix X,
                         double eps,
                         int min_samples,
                         std::string metric = "euclidean",
                         List metric_params = R_NilValue,
                         std::string algorithm = "auto",
                         int leaf_size = 30,
                         double p = 2,
                         int n_jobs = 1);

static const R_CMethodDef CEntries[] = {
    {"metric_adjusted_rand_index", (DL_FUNC) &metric_adjusted_rand_index, 0}, // 第三个参数是参数数量
    {"metric_silhouette_score", (DL_FUNC) &metric_silhouette_score, 0},
    {"util_dbscan_fit", (DL_FUNC) &util_dbscan_fit, 0},
    {NULL, NULL, 0}
};

void R_init_DBSCAN615(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
