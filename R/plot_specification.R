#' Plot the specified model to check if it is well specified. If priors are
#' specified, the function will sample from the priors and plot the model.
#' Otherwise, the function will try to find a suitable combination of
#' parameters. If the model does not compile after a reasonably large number of
#' tries, that is a good indication that the model or the priors are not well
#' specified.
#'
#' @param model A slendr model generating function.
#' @param ntries The number of tries to compile the model.
#' @param priors A optional list of prior distributions to use for sampling of
#' model parameters
#'
plot_specification <- function(model, ntries = 1000, priors = list()) {
  # Extract the parameters of the model
  params <- rlang::fn_fmls_names(model)

  # Generate a grid of parameters
  grid <- rep(NA, length(params)) |> as.list()
  names(grid) <- params

  # We evaluate the priors to (1) get the names and
  # (2) assert that they are valid
  prior_df <- purrr::map(priors, sample_prior) |>
    purrr::map(as.data.frame) |>
    dplyr::bind_rows() |>
    dplyr::mutate(rowid = dplyr::row_number())
  if (nrow(prior_df) > 0) {
    rownames(prior_df) <- prior_df$variable
  }

  # Identify all calls to population using a recursive function
  find_calls_fn <- function(expr, fn_name) {
    if (!rlang::is_call(expr)) return(NULL)
    if (rlang::is_symbol(expr[[1]], fn_name)) return(expr)
    # Check special case of using :: syntax
    if (rlang::is_call(expr[[1]], "::")) {
      check <- rlang::is_symbol(expr[[1]][[2]], "slendr") &&
        rlang::is_symbol(expr[[1]][[3]], fn_name)
      if (check) return(expr)
    }


    expr[-1] |>
      as.list() |>
      purrr::map(find_calls_fn, fn_name) |>
      purrr::compact()
  }

  rec_flatten <- function(x) {
    if (!inherits(x, "list")) return(list(x))
    else return(unlist(c(lapply(x, rec_flatten)), recursive = FALSE))
  }

  # Apply the recursive function to the body of the model function
  population_calls <- model |>
    rlang::fn_body() |>
    find_calls_fn("population") |>
    rec_flatten()

  # This function extracts from a call to the function fn
  # the argument arg, if present
  find_parameters <- function(expr, fn, arg) {
    expr |>
      rlang::call_match(fn, defaults = TRUE) |>
      rlang::call_args() |>
      purrr::pluck(arg)
  }

  # Extract the population parameters
  population_parameters <- population_calls |>
    purrr::map(\(x) find_parameters(x, slendr::population, "N")) |>
    purrr::keep(is.name) |>
    unique()

  # We can assign arbitrary values to the population parameters
  for (param in population_parameters) {
    grid[[param]] <- 100
  }

  # Now, we iteratively generate combinations until the model compiles
  # We define a helper function to guess the value of a parameter
  guess <- function(val, param) {
    if (param %in% rownames(prior_df)) {
      index <- prior_df[param, "rowid"]
      return(sample_prior(priors[[index]])$value)
    }
    if (!is.na(val)) {
      return(val)
    }
    sample.int(.Machine$integer.max, 1)
  }

  last_error <- NULL
  tempgrid <- NULL
  for (i in 1:ntries) {
    # Generate a random combination for all values that are still NA
    tempgrid <- purrr::imap(grid, guess)
    # Call model function with the current grid, and try to compile
    # If error, reset the grid
    tryCatch({
      tempmodel <- do.call(model, tempgrid)
      msg <- paste("Model compiled successfully after", i, "tries")
      print(msg)
      grid <- tempgrid
      break
    }, error = function(e) {
      last_error <<- e
      tempgrid <<- grid
    })
  }
  # Check if the model compiled
  if (!exists("tempmodel")) {
    paste("Last error:", last_error$message) |> message()
    paste("You can recreate it with the following code:") |> message()
    # Format dput correctly
    paste0("grid <- ", paste0(deparse(tempgrid), collapse = "") )|> message()
    paste("do.call(model, grid)") |> message()
    paste("Model did not compile after", ntries, "tries") |> stop()

  }
  # For better visualization, we can extract the times of the population calls
  # and give them reasonable values
  # However, it get's a bit tricky if we have gene flow
  # For know, we will only consider the case where gene flow is not present
  # and the model is forward in time
  shrink_grid <- function() {
    times <- population_calls |>
      purrr::map(\(x) find_parameters(x, slendr::population, "time"))
    # We have to decide whether direction was forward or backward
    init_time <- times |>
      purrr::keep(is.numeric) |>
      as.numeric() |>
      min()
    end_time <- tempmodel$length
    time_params <- purrr::keep(times, is.name) |> as.character()
    sorted_times <-  grid[time_params] |> as.numeric() |> rank()
    # Set up time to be equally spaced between init_time and end_time
    seq_time <- seq(init_time, end_time, length.out = length(sorted_times)+2)
    grid[time_params] <- seq_time[sorted_times+1]
    grid
  }

  # Check if there is gene flow
  gene_flow_calls <- model |>
    rlang::fn_body() |>
    find_calls_fn("gene_flow")
  any_gene_flow <- length(gene_flow_calls) > 0
  is_fwd <- tempmodel$direction == "forward"
  # Compile the final model
  compiled <- tempmodel
  if (!any_gene_flow && is_fwd) {
    grid <- shrink_grid()
    tryCatch({
      compiled <- do.call(model, grid)
    },
    # Just ignore the error
    error = function(e) {
      paste("Model did not compile after shrinking the grid") |> message()
      paste("Last error:", e$message) |> message()
    }
    )
  }
  compiled |>
    slendr::plot_model() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}
