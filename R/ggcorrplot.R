# internal
# @title Add the geom of correlation matrix to the existing plot
#
# @param p a ggplot object
# @param data a data.frame to be plotted
# @param method a character indicating the visualization method of correlation matrix to be used. Currently, it supports four methods, named "circle" (default), "square", "ellipse", "number".
#' @import ggplot2
#' @importFrom dplyr %>% mutate group_by group_modify case_when
#' @importFrom rlang .data
plot.method <- function(p, data, method = c("circle", "square", "ellipse", "number")) {
  if (method == "ellipse") {
    ellipse.xy <- function(rho, length = 100) {
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
                label = data$rho.label)
  }
  return(p)
}

#' @title Visualize a correlation matrix
#'
#' @param corr a correlation matrix to be visualized
#' @param method a character indicating the visualization method of correlation matrix to be used. Currently, it supports four methods, named \code{"circle"} (default), \code{"square"}, \code{"ellipse"}, \code{"number"}.
#' @param type a character indicating that the \code{"full"} (default), \code{"upper"} or \code{"lower"} triangular matrix is displayed.
#' @param p.mat a matrix of p-value
#' @param sig.lvl a numeric vector specifying significant level(s). If the p-value in \code{p.mat} is bigger than \code{sig.lvl} (0.05 by default), then the corresponding correlation coefficient is regarded as insignificant. If \code{insig} is \code{"label_sig"}, this may be an increasing vector of significance levels, for example \code{c(0.05, 0.01, 0.001)}, in which case \code{pch} will be used once for the highest p-value interval and multiple times (e.g. "*", "**", "***") for each lower p-value interval.
#' @param number.digits the number of decimal digits (2 by default) while the visualization method is \code{"number"}.
#' @param show.diag a logical indicating whether display the correlation coefficients on the principal diagonal.
#' @param insig a character specialized insignificant correlation coefficients, \code{"pch"} (default), \code{"blank"}, or \code{"label_sig"}.
#' @param pch a point character indicating the shape of insignificant correlation coefficients.
#' @param pch.cex a number controlling the shape size of insignificant correlation coefficients.
#' @export
ggcorrplot <- function(corr, method = c("circle", "square", "ellipse", "number"),
                       type = c("full", "lower", "upper"), p.mat = NULL,
                       sig.lvl = 0.05, number.digits = 2,
                       show.diag = TRUE, insig = c("pch", "blank", "label_sig"),
                       pch = 4, pch.cex = 5) {
  method <- match.arg(method)
  type <- match.arg(type)
  insig <- match.arg(insig)

  if (is.null(colnames(corr))) {
    stop("Correlation matrix needs column names!")
  }
  vars <- colnames(corr)
  # number of variables
  nvars <- length(vars)

  corr <- reshape2::melt(corr, value.name = "rho") %>%
    mutate(rid = as.integer(as.factor(.data$Var1)),
           cid = as.integer(as.factor(.data$Var2))) %>%
    mutate(part = case_when(
      .$rid < .$cid ~ "upper",
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    )) %>%
    mutate(abs.rho = abs(.data$rho)) %>%
    mutate(rho.label = ifelse(.data$rho == 1, .data$rho, format(round(.data$rho, digits = number.digits),
                                                                nsmall = number.digits)))

  if (!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, value.name = "pval") %>%
      mutate(rid = as.integer(as.factor(.data$Var1)),
             cid = as.integer(as.factor(.data$Var2))) %>%
      mutate(part = case_when(
        .$rid < .$cid ~ "upper",
        .$rid == .$cid ~ "diag",
        .$rid > .$cid ~ "lower"
      ))

    sig.codes <- sapply(seq_along(sig.lvl), function(i) {
      # By default, mark significance with *
      if (!is.character(pch)) {
        pch <- "*"
      }
      paste(rep(pch, i), collapse = "")
    })

    corr <- corr %>%
      mutate(pval = p.mat$pval) %>%
      mutate(signif = as.numeric(.data$pval <= max(sig.lvl))) %>%
      mutate(sig.codes = cut(.data$pval, breaks = c(sig.lvl, 0, 1), labels = c(rev(sig.codes), ""),
                             include.lowest = TRUE))

    # insignificant p value matrix
    p.mat.insig <- p.mat %>%
      dplyr::filter(.data$pval > max(sig.lvl))

    if (insig == "blank") {
      corr <- corr %>%
        mutate(rho = .data$rho * signif)
    }
  } else {
    p.mat.insig <- NULL
  }

  if (type == "lower") {
    corr <- corr %>%
      dplyr::filter(.data$part != "upper")
    if (!is.null(p.mat.insig)) {
      p.mat.insig <- p.mat.insig %>%
        dplyr::filter(.data$part != "upper")
    }
  } else if(type == "upper") {
    corr <- corr %>%
      dplyr::filter(.data$part != "lower")
    if (!is.null(p.mat.insig)) {
      p.mat.insig <- p.mat.insig %>%
        dplyr::filter(.data$part != "lower")
    }
  }

  if (!show.diag) {
    corr <- corr %>%
      dplyr::filter(.data$part != "diag")
  }

  # default palette of corrplot
  col2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = "RdBu"))

  p <- ggplot(data = corr) +
    geom_rect(mapping = aes(xmin = .data$cid - 0.5, xmax = .data$cid + 0.5,
                            ymin = .data$rid - 0.5, ymax = .data$rid + 0.5),
              color = "grey92", fill = NA) +
    coord_fixed() +
    theme_bw() +
    theme(legend.margin = margin(0, unit = 'cm'),
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

  if (any(is.na(corr$rho))) {
    corr.NA <- corr %>%
      dplyr::filter(is.na(.data$rho))
    corr <- corr %>%
      dplyr::filter(!is.na(.data$rho))
  } else {
    corr.NA <- NULL
  }

  p <- plot.method(p, data = corr, method = method)
  # add significant codes except number method
  if (!is.null(p.mat) & insig == "label_sig" & method != "number") {
    p <- p +
      geom_text(data = corr, mapping = aes(x = .data$cid, y = .data$rid), label = corr$sig.codes,
                size = pch.cex)
  }

  # indicate insigificant p value with point character
  if (!is.null(p.mat.insig) & insig == "pch") {
    p <- p + geom_point(data = p.mat.insig, mapping = aes(x = .data$cid, y = .data$rid),
                        shape = pch, size = pch.cex)
  }

  # indicate NA correlation coefficient
  if (!is.null(corr.NA)) {
    p <- p +
      geom_text(data = corr.NA, mapping = aes(x = .data$cid, y = .data$rid), label = "NA",
                size = pch.cex, color = "grey92")
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
  axis.text.fontsize <- 11 # in pt
  geom.text.fontsize <- axis.text.fontsize / ggplot2::.pt # in mm
  if (type == "full") {
    p <- p +
      scale_x_continuous(breaks = 1:nvars, labels = vars, expand = c(0, 0)) +
      scale_y_reverse(breaks = 1:nvars, labels = vars, expand = c(0, 0))
  } else if (type == "lower") {
    if (show.diag) {
      y.vars <- data.frame(x = 0, y = 1:nvars)
      diag.vars <- data.frame(x = 1:nvars, y = 0:(nvars - 1))
      y.label <- diag.label <- vars
    } else {
      y.vars <- data.frame(x = 0, y = 2:nvars)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      y.label <- utils::tail(vars, -1)
      diag.label <- utils::head(vars, -1)
    }
    p <- p +
      geom_text(data = diag.vars, mapping = aes(.data$x, .data$y), label = diag.label,
                colour = "grey30", size = geom.text.fontsize) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_reverse(breaks = y.vars$y, labels = y.label, expand = c(0, 0),
                      limits = c(nvars + 0.5, min(diag.vars$y) - 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = axis.text.fontsize))
  } else if (type == "upper") {
    if (show.diag) {
      x.vars <- data.frame(x = 1:nvars, y = 0)
      diag.vars <- data.frame(x = 0:(nvars - 1), y = 1:nvars)
      x.label <- diag.label <- vars
    } else {
      x.vars <- data.frame(x = 2:nvars, y = 0)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      x.label <- utils::tail(vars, -1)
      diag.label <- utils::head(vars, -1)
    }
    p <- p +
      geom_text(data = diag.vars, mapping = aes(.data$x, .data$y), label = diag.label,
                colour = "grey30", size = geom.text.fontsize) +
      scale_x_continuous(breaks = x.vars$x, labels = x.label, expand = c(0, 0),
                         position = "top", limits = c(min(diag.vars$x) - 0.5, nvars + 0.5)) +
      scale_y_reverse(expand = c(0, 0)) +
      theme(axis.text.x = element_text(size = axis.text.fontsize),
            axis.text.y = element_blank())
  }

  return(p)
}
