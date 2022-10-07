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
