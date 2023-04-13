#' @importFrom slendr init_env
#' @export
slendr::init_env

# Check that the function argument is really provided by the user
check_arg <- function(x) {
  tryCatch({get(deparse(substitute(x))); TRUE}, error = function(e) FALSE) ||
  tryCatch({!is.null(x); TRUE}, error = function(e) FALSE)
}

check_param_presence <- function(params, p) {
  if (length(intersect(params, p)) != length(p)) {
    missing <- setdiff(p, params)
    stop(paste(missing, collapse = ", "), " not among the estimated model parameters",
         call. = FALSE)
  }
}

# a function to silence the unnecessary summary() output on abc objects
# https://stackoverflow.com/a/54136863
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# Extract prior variable names as a character vector
prior_variables <- function(priors) {
  sapply(priors, function(p) as.character(as.list(p)[[2]]))
}

subset_parameters <- function(subset, all) {
  params <- all
  if (!is.null(subset)) {
    param_re <- paste0(subset, collapse = "|")
    params <- grep(param_re, params, value = TRUE)
    if (length(params) == 0)
      stop("No parameters fit the provided parameter subset", call. = FALSE)
  }
  params
}
