visu_plot_clusters <- function(
    X,
    title,
    labels,
    method = "pca"
) {
    # Ensure X is a data frame
    if (!is.data.frame(X)) {
        X <- as.data.frame(X)
    }
    
    # If X has more than 2 columns, perform dimensionality reduction
    if (ncol(X) > 2) {
        if (method == "pca") {
            pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
            X <- as.data.frame(pca_result$x[, 1:2]) # Keep the first 2 principal components
            colnames(X) <- c("PC1", "PC2")
        } else if (method == "tsne") {
            library(Rtsne)
            tsne_result <- Rtsne(as.matrix(X), dims = 2)
            X <- as.data.frame(tsne_result$Y)
            colnames(X) <- c("Dim1", "Dim2")
        } else {
            stop("Unsupported dimensionality reduction method. Use 'pca' or 'tsne'.")
        }
    }
    
    # Add cluster labels
    X$cluster <- factor(labels)
    
    # Create the plot
    p <- ggplot(X, aes(x = X[,1], y = X[,2], color = cluster)) +
        geom_point(size = 2, alpha = 0.8) +
        labs(title = title, x = colnames(X)[1], y = colnames(X)[2], color = "Cluster") +
        theme_minimal()
    
    print(p)
}
