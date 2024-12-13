visu_plot_core_samples <- function(
    X, 
    title,
    core_sample_indices
) { 
    data <- as.data.frame(X)
    data$is_core <- FALSE
    if (!is.null(core_sample_indices)) {
        data$is_core[core_sample_indices] <- TRUE
    }

    p <- ggplot(data, aes(x = data[,1], y = data[,2], color = is_core)) +
        geom_point(size = 2, alpha = 0.8) +
        scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
        labs(title = title, x = "Feature 1", y = "Feature 2", color = "Is Core Sample") +
        theme_minimal()
    print(p)
}