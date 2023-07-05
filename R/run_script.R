#' Run a custom tailored SLiM or Python (msprime) simulation script
#'
#' This function will take as input a path to a SLiM or Python script (detecting
#' which one of the two it is based on the script's contents) and run it on the
#' command line with model arguments formatted on the command line automatically.
#'
#' The simulation engine for the script is determined by simple heuristic: if the
#' script contains the string 'treeSeqOutput(output_path' which is mandatory for
#' any SLiM script usable by demografr, then the SLiM binary is chosen as a
#' simulation engine. Otherwise, it is assumed the script is an msprime Python
#' script.
#'
#' @param script Path to the SLiM or Python script
#' @param ... Model arguments which will be provided to the script on the command line
#'
#' @export
run_script <- function(script, ...) {
  if (!file.exists(script))
    stop("No '", engine, "' script found at '", script, "'", call. = FALSE)
  else
    script <- normalizePath(script)

  # every single valid demografr SLiM script engine *must* contain the following
  # string -- this is how we can tell whether the input script is a SLiM script
  # or an msprime Python script
  script_contents <- readLines(script)
  if (any(grepl("treeSeqOutput\\(output_path", script_contents)))
    engine <- "slim"
  else
    engine <- reticulate::py_exe()

  # compose a path to an output tree-sequence file
  output_path <- paste0(tempfile(), ".trees")

  args <- list(...)

  var_names <- names(args)
  var_values <- unname(args)
  # compose a vector of CLI arguments, either for a SLiM binary or for a Python script
  prefix <- if (engine == "slim") "-d " else "--"
  cli_args <- vapply(
    seq_along(var_names),
    function(i) paste0(prefix, var_names[i], "=", var_values[i]),
    character(1)
  )

  if (engine == "slim") { # run the SLiM script on the command line
    # add the generated output tree-sequence path to the list of CLI arguments (SLiM has a
    # peculiar way of specifying string-typed CLI arguments)
    cli_args <- c(cli_args, paste0("-d \"output_path='", output_path, "'\""))

    # compose the whole CLI command
    cli_command <- paste(engine, paste(cli_args, collapse = " "), script, collapse = " ")

    # execute the command on the shell command line
    system(cli_command)
  } else { # run the msprime Python script in the reticulate'd Python interpreter
    # add the path to the output tree sequence and collapse the whole CLI command
    cli_args <- c(cli_args, paste0("--output_path=\"", output_path, "\""))

    # compose the whole CLI command
    cli_command <- paste(engine, script, paste(cli_args, collapse = " "), collapse = " ")

    # execute the command on the Python command line
    reticulate::py_run_string(sprintf("import os; os.system(r'%s')", cli_command))
  }

  if (!file.exists(output_path))
    stop("\n======================================================================\n",
         "The provided script did not leave a tree-sequence output. Inspect the log\n",
         "output above for errors and make sure you can run your custom script \n",
         "on the command-line manually.\n\n",
         "The exact command that failed was:\n\n",
         cli_command, call. = FALSE)

  # return the path to the simulated tree sequence
  output_path
}
