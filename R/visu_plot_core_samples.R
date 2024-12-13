#' @importFrom rlang .data

visu_plot_core_samples <- function(
    X, 
    title,
    core_sample_indices
) { 
    # Ensure data is a data frame
    data <- as.data.frame(X)
    
    # Set core samples based on indices
    data$is_core <- FALSE
    if (!is.null(core_sample_indices)) {
        data$is_core[core_sample_indices] <- TRUE
    }
    
    # Rename columns for clarity
    colnames(data)[1:2] <- c("Feature1", "Feature2")
    
    # Plot
    p <- ggplot2::ggplot(
            data,
            ggplot2::aes(x = .data$Feature1, y = .data$Feature2, color = .data$is_core)
        ) +
        ggplot2::geom_point(size = 2, alpha = 0.8) +
        ggplot2::scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
        ggplot2::labs(
            title = title,
            x = "Feature 1",
            y = "Feature 2",
            color = "Is Core Sample"
        ) +
        ggplot2::theme_minimal()
    
    # Print the plot
    print(p)
}
