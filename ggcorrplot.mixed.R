rm(list = ls())

source("ggcorrplot.R")

ggcorrplot.mixed <- function(corr, upper = c("circle", "square", "ellipse", "number"), 
                             lower = c("circle", "square", "ellipse", "number"), 
                             p.mat = NULL, sig.lvl = 0.05, number.digits = 2, 
                             insig = c("pch", "blank"), 
                             pch = 4, pch.cex = 5) {
  upper <- match.arg(upper)
  lower <- match.arg(lower)
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
  
  # insignificant p value matrix
  p.mat0 <- p.mat0 %>% 
    filter(pval > sig.lvl)
  
  if (insig == "blank") {
    corr0 <- corr0 %>% 
      mutate(rho = rho * signif)
  }
  
  # default palette of corrplot
  library(RColorBrewer)
  col2 <- colorRampPalette(brewer.pal(n = 11, name = "RdBu"))
  
  library(ggplot2)
  library(ggforce)
  library(plyr)
  
  p <- ggplot(data = corr0) + 
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
  
  # add upper plot
  upper.dat <- corr0 %>% 
    filter(part == "upper")
  
  p <- plot.method(p, data = upper.dat, method = upper)
  
  # add lower plot
  lower.dat <- corr0 %>% 
    filter(part == "lower")
  
  p <- plot.method(p, data = lower.dat, method = lower)
  
  # indicate insigificant p value with point character
  if (insig == "pch") {
    p <- p + geom_point(data = p.mat0, mapping = aes(x = cid, y = rid), 
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
    geom_text(data = diag.vars, mapping = aes(x, y), label = diag.label, colour = "grey30")
  
  return(p)
}

x <- mtcars

corr <- cor(x)
p.mat <- cor.mtest(mtcars, conf.level = 0.95)

p <- ggcorrplot.mixed(corr = corr, p.mat = p.mat, upper = "ellipse", 
                      lower = "number")
p