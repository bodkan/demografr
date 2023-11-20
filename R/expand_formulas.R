# If there are any priors or bounds specified as "<par>... ~ <expression>", look for slendr
# model function arguments matching "<par>", and "instantiate" them in form of their own
# prior sampling or boundary parameter statements
expand_formulas <- function(formulas, model, model_args = NULL) {
  # get names of "fully instantiated" parameters and those containing "...",
  # which need to be matched against model function arguments
  param_names <- get_param_names(formulas)
  to_expand <- vapply(param_names, function(n) grepl("\\.\\.\\.", n), logical(1))

  # get all non-implicit arguments of the model function
  nonimpl_args <- get_nonimpl_params(model)
  # the rest of the arguments (i.e. model parameters) are missing a formula expression
  missing_args <- setdiff(nonimpl_args, c(param_names, names(model_args)))

  # attempt to match formulas to be expanded against missing model function arguments,
  # replacing all `...` statements with "fully instantiated" parameter names
  formulas_expanded <- list()
  expand_prefixes <- gsub("\\.\\.\\.", "", param_names[to_expand])
  for (arg in missing_args) {
    matches <- vapply(expand_prefixes, function(x) grepl(paste0("^", x), arg), FUN.VALUE = logical(1))
    matching_formulas <- formulas[to_expand][matches]
    if (length(matching_formulas) > 1)
      stop("Multiple matching formulas for the model parameter '", arg, "'", call. = FALSE)
    else if (length(matching_formulas) == 1) {
      expr <- as.list(matching_formulas[[1]])[[3]]
      formulas_expanded[[length(formulas_expanded) + 1]] <- stats::as.formula(paste(arg, "~", deparse(expr), collapse = " "))
    }
  }

  c(formulas[!to_expand], formulas_expanded)
}
