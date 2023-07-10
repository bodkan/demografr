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
  sapply(seq_along(priors), function(i) {
    ast <- as.list(priors[[i]])
    variable_tokens <- as.character(as.list(ast[[2]]))
    if (length(variable_tokens) == 1)
      variable_index <- 1
    else
      variable_index <- 2
    variable_tokens[[variable_index]]
  })
}

# a function to silence the unnecessary summary() output on abc objects
# https://stackoverflow.com/a/54136863
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

subset_parameters <- function(subset, all) {
  parameters <- all
  if (!is.null(subset)) {
    param_re <- paste0(subset, collapse = "|")
    parameters <- grep(param_re, parameters, value = TRUE)
    if (length(parameters) == 0)
      stop("No parameters fit the provided parameter subset or regular expression", call. = FALSE)
  }
  parameters
}

# Extract the return expression from a given R function
extract_return <- function(model) {
  # get the AST of the model function
  fun_body <- as.list(body(model))

  pos <- which(sapply(fun_body, function(x) is.call(x) && x[[1]] == quote(return)))

  if (length(pos) > 0) {
    return_expr <- fun_body[pos]
    return(return_expr)
  } else {
    return(NULL)
  }
}

check_model_functions <- function(model) {
  return_exprs <- extract_return(model)

  if (length(return_expr) != 1)
    stop("A demografr model function must have exactly one return statement", call. = FALSE)

  # extract the content of the return statement itself (i.e. for return(<expr>) gives <expr>)
  return_expr <- as.list(return_exprs[[1]])[[2]]

  # unpack the expression
}

check_model_engine <- function(model, engine) {
  engine <- match.arg(engine)
  if (inherits(model, "slendr_model") && !engine %in% c("msprime", "slim"))
    stop("For a slendr functions as a model, 'engine' must be either \"msprime\" or \"slim\"",
         call. = FALSE)
  if ((engine == "custom" && (!is.character(model) || !file.exists(model))) ||
      (!is.function(model) && engine != "custom"))
    stop("\nSetting 'engine' to \"custom\" is only allowed with a user-defined 'model' script.\n",
         "Setting slendr function as a 'model' is only allowed with \"msprime\" or \"slim\" as 'engine'.",
         call. = FALSE)
}

get_engine <- function(slendr_model, engine) {
  if (!is.null(engine)) # if an engine was specified by the user, use that
    engine <- match.arg(engine, c("msprime", "slim"))
  else if (is.null(slendr_model$world))
    engine <- "msprime" # nonspatial models should use the coalescent slendr/msprime engine
  else
    engine <- "slim" # spatial models must use the slendr/SLiM engine
  engine
}
