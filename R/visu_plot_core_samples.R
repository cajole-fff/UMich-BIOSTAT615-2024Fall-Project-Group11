#' @importFrom rlang .data

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

    p <- ggplot2::ggplot(
            data,
            ggplot2::aes(x = .data[[1]], y = .data[[2]], color = .data$is_core)
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
    print(p)
}
