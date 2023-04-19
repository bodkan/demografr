#' @importFrom slendr init_env
#' @export
slendr::init_env

# Check that the function argument is really provided by the user
check_arg <- function(x) {
  tryCatch({get(deparse(substitute(x))); TRUE}, error = function(e) FALSE) ||
  tryCatch({!is.null(x); TRUE}, error = function(e) FALSE)
}

# Extract prior variable names as a character vector
get_prior_names <- function(priors) {
  sapply(seq_along(priors), function(i) as.character(as.list(priors[[i]])[[2]]))
}

# a function to silence the unnecessary summary() output on abc objects
# https://stackoverflow.com/a/54136863
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

subset_parameters <- function(subset, all) {
  params <- all
  if (!is.null(subset)) {
    param_re <- paste0(subset, collapse = "|")
    params <- grep(param_re, params, value = TRUE)
    if (length(params) == 0)
      stop("No parameters fit the provided parameter subset or regular expression", call. = FALSE)
  }
  params
}
