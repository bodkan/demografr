#' Perform selection between different ABC models
#'
#' This function internally uses \code{abc::postpr} for computing posterior probabilities
#' of models, Bayes factors, etc., in order to aid in the selection of the most likely
#' model.
#'
#' @param models A list of objects of the class \code{demografr_sims_abc} or
#'   \code{demografr_abc.abc} which store simulated summary statistics needed for
#'   calling \code{abc::postpr}
#' @param tol,method Required arguments of \code{abc::postpr}
#' @param ... Other optional arguments to be passed to \code{abc::postpr}
#'
#' @return Object of the class \code{postpr} and \code{demografr_postpr}
#'
#' @export
model_selection <- function(models, ...) {
  args <- match.call()

  if (!"tol" %in% names(args))
      stop("The `tol` argument must be provided to use postpr function from the abc package.\n",
           "See `?abc::postpr` for more details.", call. = FALSE)
    if (!"method" %in% names(args))
      stop("The `method` argument must be provided to use postpr function from the abc package.\n",
           "See `?abc::abc` for more details.", call. = FALSE)

  if (is.null(names(models)) || any(names(models) == ""))
    stop("The 'models' argument must be a named list, with the names being\n",
         "unique identifier names of each model", call. = FALSE)

  if (!all(vapply(models, inherits, "demografr_abc_sims", FUN.VALUE = logical(1))) &&
      !all(vapply(models, inherits, "demografr_abc.abc", FUN.VALUE = logical(1))))
    stop("The list of models must be either all objects produced by the function\n",
         "`simulate_abc()` or function `run_abc()` as they store full information\n",
         "about an ABC model being run, namely the simulated summary statistics", call. = FALSE)

  model_nsims <- vapply(models, function(x) nrow(attr(x, "components")$simulated), FUN.VALUE = integer(1))
  model_stats <- lapply(models, function(x) attr(x, "components")$simulated) %>% do.call(rbind, .) %>% as.matrix()
  model_names <- lapply(seq_along(models), function(i) rep(names(models)[i], model_nsims[i])) %>% unlist()

  observed_stats <- lapply(names(attr(abcX, "components")$observed), function(stat) {
    # convert observed statistics to a matrix, either from a normal data frame
    # result (with each statistic named), or from a simple vector
    x <- attr(abcX, "components")$observed[[stat]]

    if (is.data.frame(x)) {
      # find the column with the value of a statistic `stat`
      # TODO: the last column will be numeric
      # value_col <- sapply(names(x), function(i) is.numeric(x[[i]]))
      value_col <- ncol(x)
      values <- matrix(x[, value_col, drop = TRUE], nrow = 1)
      names <- x[, !value_col, drop = FALSE] %>%
        apply(MARGIN = 1, FUN = function(row) paste(c(stat, row), collapse = "_"))
    } else {
      values <- matrix(x, nrow = 1)
      names <- paste0(stat, "_", seq_along(x))
    }
    colnames(values) <- names
    values
  }) %>% do.call(cbind, .)

  result <- abc::postpr(observed_stats, model_names, model_stats, ...)

  class(result) <- c("demografr_postpr", class(result))

  result
}