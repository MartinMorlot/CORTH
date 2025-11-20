new_dist_plot <- function(
    dist.obj, show_labels = TRUE, lab_size = NULL,
    gradient = list(low = "red", mid = "white", high = "blue")) {
    if (!inherits(dist.obj, "dist")) {
        stop("An object of class dist is required.")
    }

    dist.obj_mat <- as.matrix(dist.obj)

    rownames(dist.obj_mat) <- colnames(dist.obj_mat) <- row.names(dist.obj)

    d <- reshape2::melt(dist.obj_mat)
    p <- ggplot(d, aes_string(x = "Var1", y = "Var2")) +
        ggplot2::geom_tile(aes_string(fill = "value"))
    if (is.null(gradient$mid)) {
        p <- p + ggplot2::scale_fill_gradient(
            low = gradient$low,
            high = gradient$high
        )
    } else {
        p <- p + ggplot2::scale_fill_gradient2(
            midpoint = mean(dist.obj_mat, na.rm = T),
            low = gradient$low, mid = gradient$mid, high = gradient$high,
            space = "Lab"
        )
    }
    if (show_labels) {
        p <- p + theme(
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_text(
                angle = 45, hjust = 1,
                size = lab_size
            ), axis.text.y = element_text(size = lab_size)
        )
    } else {
        p <- p + theme(
            axis.text = element_blank(), axis.ticks = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank()
        )
    }
    return(p)
}
