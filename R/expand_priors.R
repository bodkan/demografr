# If there are any priors specified as "<par>... ~ <sampling expression>", look for slendr
# model function arguments matching "<par>", and "instantiate" them in form of their own
# prior parameter sampling statements
expand_priors <- function(model, priors, model_args = NULL) {
  # get names of "fully instantiated" priors and priors containing "...", which need to be matched
  # against model function arguments
  prior_names <- get_prior_names(priors)
  to_expand <- vapply(prior_names, function(n) grepl("\\.\\.\\.", n), logical(1))

  # get all arguments of the model function...
  all_args <- names(formals(model))
  # ... from those, extract names of arguments which don't have an implicit value ...
  nonimpl_args <- all_args[vapply(all_args, function(x) is.name(formals(model)[[x]]), logical(1))]
  # ... which means that the rest of the arguments (i.e. model parameters) are missing
  # a prior sampling statement
  missing_args <- setdiff(nonimpl_args, c(prior_names, names(model_args)))

  # attempt to match priors to be expanded against missing model function arguments,
  # replacing the matching `...` statements with "fully instantiated" prior names
  priors_expanded <- list()
  expand_prefixes <- gsub("\\.\\.\\.", "", prior_names[to_expand])
  for (arg in missing_args) {
    matches <- vapply(expand_prefixes, function(x) grepl(paste0("^", x), arg), FUN.VALUE = logical(1))
    matching_priors <- priors[to_expand][matches]
    if (length(matching_priors) > 1)
      stop("Multiple matching priors for the model parameter '", arg, "'", call. = FALSE)
    else if (length(matching_priors) == 1) {
      sampling_expr <- as.list(matching_priors[[1]])[[3]]
      priors_expanded[[length(priors_expanded) + 1]] <- stats::as.formula(paste(arg, "~", deparse(sampling_expr), collapse = " "))
    }
  }

  c(priors[!to_expand], priors_expanded)
}
