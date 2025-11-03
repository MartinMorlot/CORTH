correlation_distance_two_vectors <- function(x, y) {
    dist <- NA
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x) == length(y)) {
        x_na <- which(is.na(x))
        y_na <- which(is.na(y))

        x[y_na] <- NA
        y[x_na] <- NA
        x <- na.omit(x)
        y <- na.omit(y)
        x <- as.numeric(x)
        y <- as.numeric(y)
        if ((length(x) > 0) & (length(y) > 0)) dist <- 1 - cor(x, y)
    }
    return(dist)
}

correlation_distance_dataframe <- function(df_to_analyze) {
    n <- nrow(df_to_analyze)
    dist_df <- data.frame(matrix(NA, n, n), row.names = row.names(df_to_analyze))
    colnames(dist_df) <- row.names(dist_df)
    for (i in 1:n) {
        for (j in i:n) {
            d <- correlation_distance_two_vectors(df_to_analyze[i, ], df_to_analyze[j, ])
            if (j == i) {
                d <- 0
            }
            dist_df[i, j] <- dist_df[j, i] <- d
        }
    }
    return(dist_df)
}

make_silhouette_and_wss_pot <- function(distance, FUNcluster = NULL, k.max = 10, linecolor = "steelblue", ...) {
    v <- rep(0, k.max)
    w <- rep(0, k.max)
    for (i in 2:k.max) {
        clust <- FUNcluster(distance, i, ...)
        v[i] <- factoextra:::.get_ave_sil_width(distance, clust$cluster)
        w[i] <- factoextra:::.get_withinSS(distance, clust$cluster)
    }
    clust1 <- FUNcluster(distance, 1, ...)
    w[1] <- factoextra:::.get_withinSS(distance, clust1$cluster)
    df <- data.frame(
        clusters = as.factor(1:k.max), y = v, z = w,
        stringsAsFactors = TRUE
    )
    p_wss <- ggpubr::ggline(df,
        x = "clusters", y = "z", group = 1, color = linecolor, ylab = "Total Within Sum of Square", xlab = "Number of clusters k",
        main = "Optimal number of clusters"
    )
    p_silhouette <- ggpubr::ggline(df,
        x = "clusters", y = "y", group = 1, color = linecolor, ylab = "Average silhouette width", xlab = "Number of clusters k",
        main = "Optimal number of clusters"
    ) + ggplot2::geom_vline(
        xintercept = which.max(v), linetype = 2,
        color = linecolor
    )
    return(list(p_wss, p_silhouette))
}


need_to_be_reworked <- function(
    object, data = NULL, choose.vars = NULL, stand = TRUE,
    axes = c(1, 2), geom = c("point", "text"), repel = FALSE,
    show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "convex",
    ellipse.level = 0.95, ellipse.alpha = 0.2, shape = NULL,
    pointsize = 1.5, labelsize = 12, main = "Cluster plot", xlab = NULL,
    ylab = NULL, outlier.color = "black", outlier.shape = 19,
    outlier.pointsize = pointsize, outlier.labelsize = labelsize,
    ggtheme = theme_grey(), ...) {
    extra_args <- list(...)
    .check_axes(axes, .length = 2)
    if (!is.null(extra_args$jitter)) {
        warning("argument jitter is deprecated; please use repel = TRUE instead, to avoid overlapping of labels.",
            call. = FALSE
        )
        if (!is.null(extra_args$jitter$width) | !is.null(extra_args$jitter$height)) {
            repel <- TRUE
        }
    }
    if (!is.null(extra_args$frame)) {
        ellipse <- .facto_dep("frame", "ellipse", ellipse)
    }
    if (!is.null(extra_args$frame.type)) {
        ellipse.type <- .facto_dep(
            "frame.type", "ellipse.type",
            extra_args$frame.type
        )
    }
    if (!is.null(extra_args$frame.level)) {
        ellipse.level <- .facto_dep(
            "frame.level", "ellipse.level",
            extra_args$frame.level
        )
    }
    if (!is.null(extra_args$frame.alpha)) {
        ellipse.alpha <- .facto_dep(
            "frame.alpha", "ellipse.alpha",
            extra_args$frame.alpha
        )
    }
    if (!is.null(extra_args$title)) {
        main <- .facto_dep("title", "main", extra_args$title)
    }
    if (inherits(object, c("partition", "hkmeans", "eclust"))) {
        data <- object$data
    } else if ((inherits(object, "kmeans") & !inherits(
        object,
        "eclust"
    )) | inherits(object, "dbscan")) {
        if (is.null(data)) {
            stop("data is required for plotting kmeans/dbscan clusters")
        }
    } else if (inherits(object, "Mclust")) {
        object$cluster <- object$classification
        data <- object$data
    } else if (inherits(object, "HCPC")) {
        object$cluster <- object$call$X$clust
        data <- res.hcpc <- object
        stand <- FALSE
    } else if (inherits(object, "hcut")) {
        if (inherits(object$data, "dist")) {
            if (is.null(data)) {
                stop("The option 'data' is required for an object of class hcut.")
            }
        } else {
            data <- object$data
        }
    } else if (!is.null(object$data) & !is.null(object$cluster)) {
        data <- object$data
        cluster <- object$cluster
    } else {
        stop("Can't handle an object of class ", class(object))
    }
    if (!is.null(choose.vars)) {
        data <- data[, choose.vars, drop = FALSE]
    }
    if (stand) {
        data <- scale(data)
    }
    cluster <- as.factor(object$cluster)
    pca_performed <- FALSE
    if (inherits(data, c("matrix", "data.frame"))) {
        if (ncol(data) > 2) {
            pca <- stats::prcomp(data, scale = FALSE, center = FALSE)
            ind <- facto_summarize(pca,
                element = "ind", result = "coord",
                axes = axes
            )
            eig <- get_eigenvalue(pca)[axes, 2]
            if (is.null(xlab)) {
                xlab <- paste0("Dim", axes[1], " (", round(
                    eig[1],
                    1
                ), "%)")
            }
            if (is.null(ylab)) {
                ylab <- paste0("Dim", axes[2], " (", round(
                    eig[2],
                    1
                ), "%)")
            }
        } else if (ncol(data) == 2) {
            ind <- as.data.frame(data, stringsAsFactors = TRUE)
            ind <- cbind.data.frame(
                name = rownames(ind), ind,
                stringsAsFactors = TRUE
            )
            if (is.null(xlab)) {
                xlab <- colnames(data)[1]
            }
            if (is.null(ylab)) {
                ylab <- colnames(data)[2]
            }
            if (xlab == "x") {
                xlab <- "x value"
            }
            if (ylab == "y") {
                ylab <- "y value"
            }
        } else {
            stop("The dimension of the data < 2! No plot.")
        }
        colnames(ind)[2:3] <- c("x", "y")
        label_coord <- ind
    } else if (inherits(data, "HCPC")) {
        ind <- res.hcpc$call$X[, c(axes, ncol(res.hcpc$call$X))]
        colnames(ind) <- c("Dim.1", "Dim.2", "clust")
        ind <- cbind.data.frame(name = rownames(ind), ind, stringsAsFactors = TRUE)
        colnames(ind)[2:3] <- c("x", "y")
        label_coord <- ind
        eig <- get_eigenvalue(res.hcpc$call$t$res)[axes, 2]
        if (is.null(xlab)) {
            xlab <- paste0("Dim", axes[1], " (", round(
                eig[1],
                1
            ), "%)")
        }
        if (is.null(ylab)) {
            ylab <- paste0("Dim", axes[2], " (", round(
                eig[2],
                1
            ), "%)")
        }
    } else {
        stop("A data of class ", class(data), " is not supported.")
    }
    label <- FALSE
    if ("text" %in% geom) {
        label <- TRUE
    }
    if (!("point" %in% geom)) {
        pointsize <- 0
    }
    plot.data <- cbind.data.frame(ind, cluster = cluster, stringsAsFactors = TRUE)
    label_coord <- cbind.data.frame(label_coord,
        cluster = cluster,
        stringsAsFactors = TRUE
    )
    if (inherits(object, "Mclust")) {
        plot.data$uncertainty <- object$uncertainty
        label_coord$uncertainty <- object$uncertainty
    }
    is_outliers <- FALSE
    if (inherits(object, c("dbscan", "Mclust"))) {
        outliers <- which(cluster == 0)
        if (length(outliers) > 0) {
            is_outliers <- TRUE
            outliers_data <- plot.data[outliers, , drop = FALSE]
            outliers_labs <- label_coord[outliers, , drop = FALSE]
            ind <- ind[-outliers, , drop = FALSE]
            cluster <- cluster[-outliers]
            plot.data <- plot.data[-outliers, , drop = FALSE]
            label_coord <- label_coord[-outliers, , drop = FALSE]
        }
    }
    lab <- NULL
    if ("text" %in% geom) {
        lab <- "name"
    }
    if (is.null(shape)) {
        shape <- "cluster"
    }
    if (inherits(object, "partition") & missing(show.clust.cent)) {
        show.clust.cent <- FALSE
    }
    p <- ggpubr::ggscatter(plot.data, "x", "y",
        color = "cluster",
        shape = shape, size = pointsize, point = "point" %in%
            geom, label = lab, font.label = labelsize, repel = repel,
        mean.point = show.clust.cent, ellipse = ellipse, ellipse.type = ellipse.type,
        ellipse.alpha = ellipse.alpha, ellipse.level = ellipse.level,
        main = main, xlab = xlab, ylab = ylab, ggtheme = ggtheme,
        ...
    )
    if (is_outliers) {
        p <- .add_outliers(p, outliers_data, outliers_labs, outlier.color,
            outlier.shape, outlier.pointsize, outlier.labelsize / 3,
            geom,
            repel = repel
        )
    }
    p
}
