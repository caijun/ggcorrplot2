#' Compute the matrix of correlation p-values
#'
#' @param x numeric matrix or data frame
#' @param ... other arguments to be passed to the function cor.test.
#' @export
cor.mtest <- function(x, ...) {
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- stats::cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

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
    
    ellipse.dat <- ddply(data, .(Var1, Var2), function(df) {
      res <- ellipse.xy(df$rho) %>% 
        mutate(rid = df$rid, cid = df$cid, rho =  df$rho) %>% 
        mutate(x1 = 0.9 * x + cid, y1 = 0.9 * y + rid, 
               group = paste(rid, cid, sep = "-"))
    })
    
    p <- p + 
      geom_polygon(data = ellipse.dat, mapping = aes(x = x1, y = y1, fill = rho, group = group), 
                   color = NA)
  } else if (method == "circle") {
    p <- p + 
      geom_circle(data = data, mapping = aes(x0 = cid, y0 = rid, r = abs.rho/2 - 0.02, fill = rho), 
                  color = NA)
  } else if (method == "square") {
    p <- p + 
      geom_rect(data = data, mapping = aes(xmin = cid - 0.5*(abs.rho - 0.04), 
                                           xmax = cid + 0.5*(abs.rho - 0.04), 
                                           ymin = rid - 0.5*(abs.rho - 0.04), 
                                           ymax = rid + 0.5*(abs.rho - 0.04), 
                                           fill = rho))
  } else if (method == "number") {
    p <- p + 
      geom_text(data = data, mapping = aes(x = cid, y = rid, colour = rho), 
                label = data$num.label, alpha = data$abs.rho)
  }
  return(p)
}

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
  
  require(dplyr)
  p.mat0 <- reshape2::melt(p.mat, value.name = "pval") %>% 
    mutate(rid = as.integer(as.factor(Var1)), 
           cid = as.integer(as.factor(Var2))) %>% 
    mutate(part = case_when(
      .$rid < .$cid ~ "upper", 
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    ))
  
  corr0 <- reshape2::melt(corr, value.name = "rho") %>% 
    mutate(rid = as.integer(as.factor(Var1)), 
           cid = as.integer(as.factor(Var2))) %>% 
    mutate(part = case_when(
      .$rid < .$cid ~ "upper", 
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    )) %>% 
    mutate(pval = p.mat0$pval) %>% 
    mutate(signif = as.numeric(pval <= sig.lvl)) %>% 
    mutate(abs.rho = abs(rho)) %>% 
    mutate(num.label = ifelse(rho == 1, rho, format(round(rho, digits = number.digits), 
                                                    nsmall = number.digits)))
  
  if (type == "lower") {
    corr <- corr0 %>% 
      filter(part != "upper")
    p.mat <- p.mat0 %>% 
      filter(part != "upper")
  } else if(type == "upper") {
    corr <- corr0 %>% 
      filter(part != "lower")
    p.mat <- p.mat0 %>% 
      filter(part != "lower")
  } else {
    corr <- corr0
    p.mat <- p.mat0
  }
  
  if (!show.diag) {
    corr <- corr %>% 
      filter(part != "diag")
    p.mat <- p.mat %>% 
      filter(part != "diag")
  }
  
  # insignificant p value matrix
  p.mat <- p.mat %>% 
    filter(pval > sig.lvl)
  
  if (insig == "blank") {
    corr <- corr %>% 
      mutate(rho = rho * signif)
  }
  
  # default palette of corrplot
  library(RColorBrewer)
  col2 <- colorRampPalette(brewer.pal(n = 11, name = "RdBu"))
  # customize your own palette
  colorRampPalette(c("red", "white", "blue"))(200)
  
  library(ggplot2)
  library(ggforce)
  library(plyr)
  
  p <- ggplot(data = corr) + 
    geom_rect(mapping = aes(xmin = cid - 0.5, xmax = cid + 0.5, 
                            ymin = rid - 0.5, ymax = rid + 0.5), 
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
  
  p <- plot.method(p, data = corr, method = method)
  
  # indicate insigificant p value with point character
  if (insig == "pch") {
    p <- p + geom_point(data = p.mat, mapping = aes(x = cid, y = rid), 
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
      geom_text(data = x.vars, mapping = aes(x, y), label = vars, colour = "grey30") + 
      geom_text(data = y.vars, mapping = aes(x, y), label = vars, colour = "grey30")
  } else if (type == "lower") {
    if (show.diag) {
      y.vars <- data.frame(x = 0, y = 1:nvars)
      diag.vars <- data.frame(x = 1:nvars, y = (1:nvars) - 1)
      y.label <- diag.label <- vars
    } else {
      y.vars <- data.frame(x = 0, y = 2:nvars)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      y.label <- tail(vars, -1)
      diag.label <- head(vars, -1)
    }
    p <- p + 
      geom_text(data = y.vars, mapping = aes(x, y), label = y.label, colour = "grey30") + 
      geom_text(data = diag.vars, mapping = aes(x, y), label = diag.label, colour = "grey30")
  } else if (type == "upper") {
    if (show.diag) {
      x.vars <- data.frame(x = 1:nvars, y = 0)
      diag.vars <- data.frame(x = (1:nvars) - 1, y = 1:nvars)
      x.label <- diag.label <- vars
    } else {
      x.vars <- data.frame(x = 2:nvars, y = 0)
      diag.vars <- data.frame(x = 1:(nvars - 1), y = 1:(nvars - 1))
      x.label <- tail(vars, -1)
      diag.label <- head(vars, -1)
    }
    p <- p + 
      geom_text(data = x.vars, mapping = aes(x, y), label = x.label, colour = "grey30") + 
      geom_text(data = diag.vars, mapping = aes(x, y), label = diag.label, colour = "grey30")
  }
  
  return(p)
}
