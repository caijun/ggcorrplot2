rm(list = ls())

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

x <- mtcars

corr <- cor(x)
p.mat <- cor.mtest(mtcars, conf.level = 0.95)

# parameters
method = "ellipse"
# method = "square"
# method = "circle"
# method = "number"
number.digits = 2
type = "full"
# type = "lower"
# type = "upper"
show.diag = TRUE
sig.level = 0.05
insig = "blank"
insig = "pch"
pch = 4
pch.cex = 5

vars <- colnames(corr)
# number of variables
nvars <- length(vars)

if (type == "lower") {
  corr[upper.tri(corr)] <- NA
  p.mat[upper.tri(p.mat)] <- NA
} else if(type == "upper") {
  corr[lower.tri(corr)] <- NA
  p.mat[lower.tri(p.mat)] <- NA
}

if (!show.diag) {
  diag(corr) <- NA
  diag(p.mat) <- NA
}

library(dplyr)

p.mat <- reshape2::melt(p.mat, na.rm = TRUE) %>% 
  mutate(Var1i = as.integer(as.factor(Var1)), 
         Var2i = as.integer(as.factor(Var2)))
corr <- reshape2::melt(corr, na.rm = TRUE) %>% 
  dplyr::rename(rho = value) %>% 
  mutate(pval = p.mat$value) %>% 
  mutate(signif = as.numeric(pval <= sig.level)) %>% 
  mutate(abs.rho = abs(rho)) %>% 
  mutate(num.label = ifelse(rho == 1, rho, format(round(rho, digits = number.digits), 
                                                    nsmall = number.digits))) %>% 
  mutate(Var1i = as.integer(as.factor(Var1)), 
         Var2i = as.integer(as.factor(Var2)))
# insignificant p value matrix
p.mat <- p.mat %>% 
  filter(value > sig.level)

if (insig == "blank") {
  corr <- corr %>% 
    mutate(rho = rho * signif)
}

# default palette of corrplot
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
# customize your own palette
colorRampPalette(c("red","white","blue"))(200)

library(ggplot2)
library(ggforce)
library(plyr)

p <- ggplot(data = corr)

if (method == "ellipse") {
  ellipse.xy <- function(rho, length = 99) {
    theta <- seq(0, 2 * pi, length = length)
    if (rho == 1) rho <- rho - 1e-4
    d <- acos(rho)
    x <- cos(theta + d / 2) / 2
    y <- cos(theta - d / 2) / 2
    as.data.frame(cbind(x, y))
  }
  
  ellipse.dat <- ddply(corr, .(Var1, Var2), function(df) {
    res <- ellipse.xy(df$rho) %>% 
      mutate(Var1i = df$Var1i, Var2i = df$Var2i, rho =  df$rho) %>% 
      mutate(x1 = 0.9 * x + Var1i, y1 = 0.9 * y + Var2i, 
             group = paste(Var1i, Var2i, sep = "-"))
  })
  
  p <- p + 
    geom_polygon(data = ellipse.dat, mapping = aes(x = x1, y = y1, fill = rho, group = group), 
                 color = NA)
} else if (method == "circle") {
  p <- p + 
    geom_circle(mapping = aes(x0 = Var1i, y0 = Var2i, r = abs.rho/2 - 0.02, fill = rho), 
                color = NA)
} else if (method == "square") {
  p <- p + 
    geom_rect(mapping = aes(xmin = Var1i - 0.5*abs.rho, xmax = Var1i + 0.5*abs.rho, 
                            ymin = Var2i - 0.5*abs.rho, ymax = Var2i + 0.5*abs.rho, 
                            fill = rho))
} else if (method == "number") {
  p <- p + geom_text(mapping = aes(x = Var1i, y = Var2i, colour = rho), 
                     label = corr$num.label, alpha = corr$abs.rho)
}

if (method %in% c("ellipse", "circle", "square")) {
  p <- p + scale_fill_gradientn(colours = col2(200), limits = c(-1, 1),
                                guide = guide_colorbar(
                                  title = "",
                                  nbin = 1000,
                                  ticks.colour = "black",
                                  frame.colour = "black",
                                  barwidth = 1.5,
                                  barheight = 10))
} else {
  p <- p + scale_colour_gradientn(colours = col2(200), limits = c(-1, 1), 
                                  guide = guide_colorbar(
                                    title = "", 
                                    nbin = 1000, 
                                    ticks.colour = "black",
                                    frame.colour = "black",
                                    barwidth = 1.5, 
                                    barheight = 10))
}

# indicate insigificant p value with point character
if (insig == "pch") {
  p <- p + geom_point(data = p.mat, mapping = aes(x = Var1i, y = Var2i), 
                      shape = pch, size = pch.cex)
}

p <- p + 
  geom_vline(xintercept = 0.5 + seq(1, nvars - 1, by = 1), color = "grey92") + 
  geom_hline(yintercept = 0.5 + seq(1, nvars - 1, by = 1), color = "grey92") + 
  coord_fixed() + 
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, nvars + 0.5), 
                     breaks = seq(1, nvars, by = 1), labels = vars) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, nvars + 0.5), 
                     breaks = seq(1, nvars, by = 1), labels = vars) + 
  theme_bw() + 
  theme(axis.title = element_blank(), 
        panel.grid.major = element_blank())

p