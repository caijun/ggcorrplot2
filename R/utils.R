#' @title Compute the matrix of correlation p-values
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
