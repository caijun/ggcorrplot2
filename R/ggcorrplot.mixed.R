#' Visualize a correlation matrix using mixed methods
#'
#' @param corr a correlation matrix to be visualized
#' @param upper a character indicating the visualization method of the upper triangular matrix to be used. Currently, it supports four methods, named "circle" (default), "square", "ellipse", "number".
#' @param lower a character indicating the visualization method of the lower triangular matrix to be used. Currently, it supports four methods, named "circle", "square", "ellipse", "number"(default).
#' @param p.mat a matrix of p-value
#' @param sig.lvl significant level, which is 0.05 by default.
#' @param number.digits the number of decimal digits while the visualization method is "number".
#' @param insig a character specialized insignificant correlation coefficients, "pch" (default), or "blank".
#' @param pch a point character indicating the shape of insignificant correlation coefficients.
#' @param pch.cex a number controlling the shape size of insignificant correlation coefficients.
#' @export
ggcorrplot.mixed <- function(corr, upper = c("circle", "square", "ellipse", "number"),
                             lower = c("number", "square", "ellipse", "circle"),
                             p.mat = NULL, sig.lvl = 0.05, number.digits = 2,
                             insig = c("pch", "blank"),
                             pch = 4, pch.cex = 5) {
  upper <- match.arg(upper)
  lower <- match.arg(lower)
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

  # add upper plot
  upper.dat <- corr0 %>%
    dplyr::filter(.data$part == "upper")

  p <- plot.method(p, data = upper.dat, method = upper)

  # add lower plot
  lower.dat <- corr0 %>%
    dplyr::filter(.data$part == "lower")

  p <- plot.method(p, data = lower.dat, method = lower)

  # indicate insigificant p value with point character
  if (insig == "pch") {
    p <- p + geom_point(data = p.mat0, mapping = aes(x = .data$cid, y = .data$rid),
                        shape = pch, size = pch.cex)
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
