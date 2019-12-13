#' Add the geom of correlation matrix to the existing plot
#'
#' @param p a ggplot object
#' @param data a data.frame to be plotted
#' @param method a character indicating the visualization method of correlation matrix to be used. Currently, it supports four methods, named "circle" (default), "square", "ellipse", "number".
#' @import ggplot2
#' @importFrom dplyr %>% mutate group_by group_modify case_when
#' @importFrom rlang .data
plot.method <- function(p, data, method = c("circle", "square", "ellipse", "number")) {
  if (method == "ellipse") {
    ellipse.xy <- function(rho, length = 99) {
      theta <- seq(0, 2 * pi, length = length)
      if (rho == 1) rho <- rho - 1e-4
      d <- acos(rho)
      x <- cos(theta + d / 2) / 2
      y <- cos(theta - d / 2) / 2
      as.data.frame(cbind(x, y))
    }

    myfun <- function(df) {
      res <- ellipse.xy(df$rho) %>%
        mutate(rid = df$rid, cid = df$cid, rho =  df$rho) %>%
        # using .data prevents R CMD check from giving a NOTE about undefined global variable
        mutate(x1 = 0.9 * .data$x + .data$cid, y1 = 0.9 * .data$y + .data$rid,
               group = paste(.data$rid, .data$cid, sep = "-"))
    }

    ellipse.dat <- data %>%
      group_by(.data$Var1, .data$Var2) %>%
      group_modify(~myfun(.x))

    p <- p +
      geom_polygon(data = ellipse.dat,
                   mapping = aes(x = .data$x1, y = .data$y1, fill = .data$rho,
                                 group = .data$group),
                   color = NA)
  } else if (method == "circle") {
    p <- p +
      ggforce::geom_circle(data = data,
                           mapping = aes(x0 = .data$cid, y0 = .data$rid,
                                         r = .data$abs.rho/2 - 0.02, fill = .data$rho),
                           color = NA)
  } else if (method == "square") {
    p <- p +
      geom_rect(data = data, mapping = aes(xmin = .data$cid - 0.5*(.data$abs.rho - 0.04),
                                           xmax = .data$cid + 0.5*(.data$abs.rho - 0.04),
                                           ymin = .data$rid - 0.5*(.data$abs.rho - 0.04),
                                           ymax = .data$rid + 0.5*(.data$abs.rho - 0.04),
                                           fill = .data$rho))
  } else if (method == "number") {
    p <- p +
      geom_text(data = data, mapping = aes(x = .data$cid, y = .data$rid, colour = .data$rho),
                label = data$num.label, alpha = data$abs.rho)
  }
  return(p)
}

#' Visualize a correlation matrix
#'
#' @param corr a correlation matrix to be visualized
#' @param method a character indicating the visualization method of correlation matrix to be used. Currently, it supports four methods, named "circle" (default), "square", "ellipse", "number".
#' @param type a character indicating that "full" (default) matrix, "upper" or "lower" triangular matrix is displayed.
#' @param p.mat a matrix of p-value
#' @param sig.lvl significant level, which is 0.05 by default.
#' @param number.digits the number of decimal digits while the visualization method is "number".
#' @param show.diag a logical indicating whether display the correlation coefficients on the principal diagonal.
#' @param insig a character specialized insignificant correlation coefficients, "pch" (default), or "blank".
#' @param pch a point character indicating the shape of insignificant correlation coefficients.
#' @param pch.cex a number controlling the shape size of insignificant correlation coefficients.
#' @export
ggcorrplot <- function(corr, method = c("circle", "square", "ellipse", "number"),
                       type = c("full", "lower", "upper"), p.mat = NULL,
                       sig.lvl = 0.05, number.digits = 2,
                       show.diag = TRUE, insig = c("pch", "blank"),
                       pch = 4, pch.cex = 5) {
  method <- match.arg(method)
  type <- match.arg(type)
  insig <- match.arg(insig)

  vars <- colnames(corr)
  # number of variables
  nvars <- length(vars)

  p.mat0 <- reshape2::melt(p.mat, value.name = "pval") %>%
    mutate(rid = as.integer(as.factor(.data$Var1)),
           cid = as.integer(as.factor(.data$Var2))) %>%
    mutate(part = case_when(
      .$rid < .$cid ~ "upper",
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    ))

  corr0 <- reshape2::melt(corr, value.name = "rho") %>%
    mutate(rid = as.integer(as.factor(.data$Var1)),
           cid = as.integer(as.factor(.data$Var2))) %>%
    mutate(part = case_when(
      .$rid < .$cid ~ "upper",
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    )) %>%
    mutate(pval = p.mat0$pval) %>%
    mutate(signif = as.numeric(.data$pval <= sig.lvl)) %>%
    mutate(abs.rho = abs(.data$rho)) %>%
    mutate(num.label = ifelse(.data$rho == 1, .data$rho, format(round(.data$rho, digits = number.digits),
                                                    nsmall = number.digits)))

  if (type == "lower") {
    corr0 <- corr0 %>%
      dplyr::filter(.data$part != "upper")
    p.mat0 <- p.mat0 %>%
      dplyr::filter(.data$part != "upper")
  } else if(type == "upper") {
    corr0 <- corr0 %>%
      dplyr::filter(.data$part != "lower")
    p.mat0 <- p.mat0 %>%
      dplyr::filter(.data$part != "lower")
  }

  if (!show.diag) {
    corr0 <- corr0 %>%
      dplyr::filter(.data$part != "diag")
    p.mat0 <- p.mat0 %>%
      dplyr::filter(.data$part != "diag")
  }

  # insignificant p value matrix
  p.mat0 <- p.mat0 %>%
    dplyr::filter(.data$pval > sig.lvl)

  if (insig == "blank") {
    corr0 <- corr0 %>%
      mutate(rho = .data$rho * signif)
  }

  # default palette of corrplot
  col2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = "RdBu"))

  p <- ggplot(data = corr0) +
    geom_rect(mapping = aes(xmin = .data$cid - 0.5, xmax = .data$cid + 0.5,
                            ymin = .data$rid - 0.5, ymax = .data$rid + 0.5),
              color = "grey92", fill = NA) +
    coord_fixed() +
    scale_y_reverse() +
    theme_bw() +
    theme(legend.margin = margin(0, unit='cm'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if (!show.diag & type == "full") {
    p <- p +
      geom_rect(mapping = aes(xmin = 1 - 0.5, xmax = 1 + 0.5,
                              ymin = 1 - 0.5, ymax = 1 + 0.5),
                color = "grey92", fill = NA) +
      geom_rect(mapping = aes(xmin = nvars - 0.5, xmax = nvars + 0.5,
                              ymin = nvars - 0.5, ymax = nvars + 0.5),
                color = "grey92", fill = NA)
  }

  p <- plot.method(p, data = corr0, method = method)

  # indicate insigificant p value with point character
  if (insig == "pch") {
    p <- p + geom_point(data = p.mat0, mapping = aes(x = .data$cid, y = .data$rid),
                        shape = pch, size = pch.cex)
  }

  # colorbar
  if (type %in% c("full", "upper")) {
    if (method %in% c("ellipse", "circle", "square")) {
      p <- p + scale_fill_gradientn(colours = col2(200), limits = c(-1, 1),
                                    guide = guide_colorbar(
                                      title = "",
                                      nbin = 1000,
                                      ticks.colour = "black",
                                      frame.colour = "black",
                                      barwidth = 1.5,
                                      barheight = 15))
    } else {
      p <- p + scale_colour_gradientn(colours = col2(200), limits = c(-1, 1),
                                      guide = guide_colorbar(
                                        title = "",
                                        nbin = 1000,
                                        ticks.colour = "black",
                                        frame.colour = "black",
                                        barwidth = 1.5,
                                        barheight = 15))
    }
  } else if (type %in% c("lower")) {
    if (method %in% c("ellipse", "circle", "square")) {
      p <- p + scale_fill_gradientn(colours = col2(200), limits = c(-1, 1),
                                    guide = guide_colorbar(
                                      direction = "horizontal",
                                      title = "",
                                      nbin = 1000,
                                      ticks.colour = "black",
                                      frame.colour = "black",
                                      barwidth = 15,
                                      barheight = 1.5))
    } else {
      p <- p + scale_colour_gradientn(colours = col2(200), limits = c(-1, 1),
                                      guide = guide_colorbar(
                                        direction = "horizontal",
                                        title = "",
                                        nbin = 1000,
                                        ticks.colour = "black",
                                        frame.colour = "black",
                                        barwidth = 15,
                                        barheight = 1.5))
    }
    p <- p + theme(legend.position = "bottom")
  }

  # variable labels
  if (type == "full") {
    x.vars <- data.frame(x = 1:nvars, y = 0)
    y.vars <- data.frame(x = 0, y = 1:nvars)
    p <- p +
      geom_text(data = x.vars, mapping = aes(.data$x, .data$y), label = vars, colour = "grey30") +
      geom_text(data = y.vars, mapping = aes(.data$x, .data$y), label = vars, colour = "grey30")
  } else if (type == "lower") {
    if (show.diag) {
      y.vars <- data.frame(x = 0, y = 1:nvars)
      diag.vars <- data.frame(x = 1:nvars, y = (1:nvars) - 1)
      y.label <- diag.label <- vars
    } else {
      y.vars <- data.frame(x = 0, y = 2:nvars)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      y.label <- utils::tail(vars, -1)
      diag.label <- utils::head(vars, -1)
    }
    p <- p +
      geom_text(data = y.vars, mapping = aes(.data$x, .data$y), label = y.label, colour = "grey30") +
      geom_text(data = diag.vars, mapping = aes(.data$x, .data$y), label = diag.label, colour = "grey30")
  } else if (type == "upper") {
    if (show.diag) {
      x.vars <- data.frame(x = 1:nvars, y = 0)
      diag.vars <- data.frame(x = (1:nvars) - 1, y = 1:nvars)
      x.label <- diag.label <- vars
    } else {
      x.vars <- data.frame(x = 2:nvars, y = 0)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      x.label <- utils::tail(vars, -1)
      diag.label <- utils::head(vars, -1)
    }
    p <- p +
      geom_text(data = x.vars, mapping = aes(.data$x, .data$y), label = x.label, colour = "grey30") +
      geom_text(data = diag.vars, mapping = aes(.data$x, .data$y), label = diag.label, colour = "grey30")
  }

  return(p)
}
