#' Print a brief summary of the result of the \code{abc::cv4postpr} function
#'
#' @param x An object of the class \code{demografr_cv}, generated by \code{run_cv}
#'
#' @return Used exclusively for printing
#'
#' @export print.demografr_cv_modsel
#' @export
#' @keywords internal
print.demografr_cv_modsel <- function(x, ...) {
  class(x) <- "cv4postpr"
  print(summary(x))
}
