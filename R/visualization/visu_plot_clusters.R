visu_plot_clusters <- function(
    X,
    title,
    labels
) {
    data <- as.data.frame(X)
    data$cluster <- factor(labels)

    p <- ggplot(data, aes(x = data[,1], y = data[,2], color = cluster)) +
        geom_point(size = 2, alpha = 0.8) +
        labs(title = title, x = "Feature 1", y = "Feature 2", color = "Cluster") +
        theme_minimal()
    print(p)
}