#' Normalize function
#' @param x a vector of numeric values
#' @param method different methods to normalize the vector
#' @return a normalized vector
#' @export
normalize <- function(x, method) {
  if (method == "original") {
    x <- (x - min(x)) / (max(x) - min(x))
  }
  if (method == "log") {
    x <- log(x)
    x <- (x - min(x)) / (max(x) - min(x))
  }
  if (method == "exp") {
    x <- exp(x)
    x <- (x - min(x)) / (max(x) - min(x))
  }
  if (method == "normal") {
    x <- bestNormalize::bestNormalize(x, allow_lambert_s = TRUE)$x.t
    x <- x - mean(x)
    x <- x / (2 * max(abs(c(max(x), min(x))))) + .5
  }
  if (method == "binarize") {
    x <- bestNormalize::binarize(x)$x.t
  }
  if (method == "skewed") {
    x <- 1 / (max(x + 1) - x)
    x <- (x - min(x)) / (max(x) - min(x))
  }
  x
}
