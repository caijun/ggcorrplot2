#' @title Visualize a correlation matrix using mixed methods
#'
#' @param corr a correlation matrix to be visualized
#' @param upper a character indicating the visualization method of the upper triangular matrix to be used. Currently, it supports four methods, named \code{"circle"} (default), \code{"square"}, \code{"ellipse"}, \code{"number"}.
#' @param lower a character indicating the visualization method of the lower triangular matrix to be used. Currently, it supports four methods, named \code{"circle"}, \code{"square"}, \code{"ellipse"}, \code{"number"}(default).
#' @param col a vector of the colors to be used, which are distributed uniformly from -1 to 1. If NULL, col will be set to \code{RColorBrewer::brewer.pal(n = 11, name = "RdBu")}, the default colour scheme of `corrplot`.
#' @param p.mat a matrix of p-value
#' @param sig.lvl a numeric vector specifying significant level(s). If the p-value in \code{p.mat} is bigger than \code{sig.lvl} (0.05 by default), then the corresponding correlation coefficient is regarded as insignificant. If \code{insig} is \code{"label_sig"}, this may be an increasing vector of significance levels, for example \code{c(0.05, 0.01, 0.001)}, in which case \code{pch} will be used once for the highest p-value interval and multiple times (e.g. "*", "**", "***") for each lower p-value interval.
#' @param number.digits the number of decimal digits (2 by default) while the visualization method is \code{"number"}.
#' @param insig a character specialized insignificant correlation coefficients, \code{"pch"} (default), \code{"blank"}, or \code{"label_sig"}.
#' @param pch a point character indicating the shape of insignificant correlation coefficients.
#' @param pch.cex a number controlling the shape size of insignificant correlation coefficients.
#' @export
ggcorrplot.mixed <- function(corr, upper = c("circle", "square", "ellipse", "number"),
                             lower = c("number", "square", "ellipse", "circle"),
                             col = NULL,
                             p.mat = NULL, sig.lvl = 0.05, number.digits = 2,
                             insig = c("pch", "blank", "label_sig"),
                             pch = 4, pch.cex = 5) {
  upper <- match.arg(upper)
  lower <- match.arg(lower)
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

  if(is.null(col)) {
    # default palette of corrplot
    col <- RColorBrewer::brewer.pal(n = 11, name = "RdBu")
  }
  col2 <- grDevices::colorRampPalette(col)


  p <- ggplot(data = corr) +
    geom_rect(mapping = aes(xmin = .data$cid - 0.5, xmax = .data$cid + 0.5,
                            ymin = .data$rid - 0.5, ymax = .data$rid + 0.5),
              color = "grey92", fill = NA) +
    coord_fixed() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(expand = c(0, 0)) +
    theme_bw() +
    theme(legend.margin = margin(0, unit = 'cm'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if (any(is.na(corr$rho))) {
    corr.NA <- corr %>%
      dplyr::filter(is.na(.data$rho))
    corr <- corr %>%
      dplyr::filter(!is.na(.data$rho))
  } else {
    corr.NA <- NULL
  }

  # add upper plot
  upper.dat <- corr %>%
    dplyr::filter(.data$part == "upper")

  p <- plot.method(p, data = upper.dat, method = upper)
  # add significant codes except number method
  if (!is.null(p.mat) & insig == "label_sig" & upper != "number") {
    p <- p +
      geom_text(data = upper.dat, mapping = aes(x = .data$cid, y = .data$rid), label = upper.dat$sig.codes,
                size = pch.cex)
  }

  # add lower plot
  lower.dat <- corr %>%
    dplyr::filter(.data$part == "lower")

  p <- plot.method(p, data = lower.dat, method = lower)
  # add significant codes except number method
  if (!is.null(p.mat) & insig == "label_sig" & lower != "number") {
    p <- p +
      geom_text(data = lower.dat, mapping = aes(x = .data$cid, y = .data$rid), label = upper.dat$sig.codes,
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
  p <- p + scale_fill_gradientn(colours = col2(200), limits = c(-1, 1),
                                guide = guide_colorbar(
                                  title = "",
                                  nbin = 1000,
                                  ticks.colour = "black",
                                  frame.colour = "black",
                                  barwidth = 1.5,
                                  barheight = 15)) +
    scale_colour_gradientn(colours = col2(200), limits = c(-1, 1),
                           guide = guide_colorbar(
                             title = "",
                             nbin = 1000,
                             ticks.colour = "black",
                             frame.colour = "black",
                             barwidth = 1.5,
                             barheight = 15))

  diag.vars <- data.frame(x = 1:nvars, y = 1:nvars)
  diag.label <- vars
  p <- p +
    geom_text(data = diag.vars, mapping = aes(.data$x, .data$y), label = diag.label, colour = "grey30")

  return(p)
}
