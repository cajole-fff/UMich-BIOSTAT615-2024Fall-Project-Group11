% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_silhouette_score.R
\name{metric_silhouette_score}
\alias{metric_silhouette_score}
\title{Silhouette Score Metric}
\usage{
metric_silhouette_score(X, labels)
}
\arguments{
\item{X}{matrix, a numeric matrix of shape (n_samples, n_features), representing the input data to be clustered.}

\item{labels}{An integer vector of cluster labels.}
}
\value{
A numeric value representing the Silhouette Score.
}
\description{
Computes the Silhouette Score for clustering evaluation.
}
