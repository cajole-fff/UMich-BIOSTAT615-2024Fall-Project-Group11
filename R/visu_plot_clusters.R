#' @importFrom rlang .data

visu_plot_clusters <- function(
    X,
    title,
    labels,
    method,
    col_names
) {
    if (!is.data.frame(X)) {
        X <- as.data.frame(X)
    }
    
    if (ncol(X) > 2) {
        if (method == "pca") {
            pca_result <- stats::prcomp(X, center = TRUE, scale. = TRUE)
            X <- as.data.frame(pca_result$x[, 1:2]) # Keep the first 2 principal components
            if (is.null(col_names)) {
                col_names <- c("PC1", "PC2")
            }
        } else if (method == "tsne") {
            tsne_result <- Rtsne::Rtsne(as.matrix(X), dims = 2)
            X <- as.data.frame(tsne_result$Y)
            if (is.null(col_names)) {
                col_names <- c("t-SNE1", "t-SNE2")
            }
        } else {
            stop("Unsupported dimensionality reduction method. Use 'pca' or 'tsne'.")
        }
    }
    
    X$cluster <- factor(labels)
    
    p <- ggplot2::ggplot(
            X,
            ggplot2::aes(x = .data[[1]], y = .data[[2]], color = .data$cluster)
        ) +
        ggplot2::geom_point(size = 2, alpha = 0.8) +
        ggplot2::labs(title = title, x = col_names[1], y = col_names[2], color = "Cluster") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            axis.title = ggplot2::element_text(size = 16),
            axis.text = ggplot2::element_text(size = 14),
            legend.title = ggplot2::element_text(size = 16),
            legend.text = ggplot2::element_text(size = 14)
        )
    
    print(p)
}
