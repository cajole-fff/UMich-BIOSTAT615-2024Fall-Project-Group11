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
            pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
            X <- as.data.frame(pca_result$x[, 1:2]) # Keep the first 2 principal components
            if (is.null(col_names)) {
                col_names <- c("PC1", "PC2")
            }
        } else if (method == "tsne") {
            library(Rtsne)
            tsne_result <- Rtsne(as.matrix(X), dims = 2)
            X <- as.data.frame(tsne_result$Y)
            if (is.null(col_names)) {
                col_names <- c("t-SNE1", "t-SNE2")
            }
        } else {
            stop("Unsupported dimensionality reduction method. Use 'pca' or 'tsne'.")
        }
    }
    
    X$cluster <- factor(labels)
    
    p <- ggplot(X, aes(x = X[,1], y = X[,2], color = cluster)) +
        geom_point(size = 2, alpha = 0.8) +
        labs(title = title, x = col_names[1], y = col_names[2], color = "Cluster") +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)
        )
    
    print(p)
}
