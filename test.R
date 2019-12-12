rm(list = ls())

source("ggcorrplot.mixed.R")

# test
corr <- cor(mtcars)
p.mat <- cor.mtest(mtcars, conf.level = 0.95)

p <- ggcorrplot(corr = corr, p.mat = p.mat, method = "ellipse", type = "full", 
                show.diag = TRUE)
p

p <- ggcorrplot.mixed(corr = corr, p.mat = p.mat, upper = "ellipse", 
                      lower = "number")
p