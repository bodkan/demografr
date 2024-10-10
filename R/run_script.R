# Run a custom tailored SLiM or Python (msprime) simulation script
#
# This function will take as input a path to a SLiM or Python script (detecting
# which one of the two it is based on the script's contents) and run it on the
# command line with model arguments formatted on the command line automatically.
run_script <- function(script, path, ...) {
  if (!file.exists(script))
    stop("No '", engine, "' script found at '", script, "'", call. = FALSE)
  else
    script <- normalizePath(script)

  # every single valid demografr SLiM script engine must contain the following
  # string -- this is how we can tell whether the input script is a SLiM script
  # or an msprime Python script
  script_contents <- readLines(script)
  if (any(grepl("initializeMutationRate", script_contents)))
    engine <- "slim"
  else
    engine <- reticulate::py_exe()

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
    # add the generated output path to the list of CLI arguments for SLiM
    cli_args <- c(cli_args, paste0("-d \"path='", path, "'\""))

    # compose the whole CLI command
    cli_command <- paste(engine, paste(cli_args, collapse = " "), script, collapse = " ")

    # execute the command on the shell command line
    system(cli_command, intern = TRUE)
  } else { # run the msprime Python script in the reticulate'd Python interpreter
    # add the path to the output tree sequence and collapse the whole CLI command
    cli_args <- c(cli_args, paste0("--path=\"", path, "\""))

    # compose the whole CLI command
    cli_command <- paste(c(engine, script, cli_args), collapse = " ")

    # execute the command on the Python command line
    reticulate::py_run_string(sprintf("import os; os.system(r'%s')", cli_command))
  }

  if (dir(path) == 0)
    stop("The provided simulation script did not generate any files. Inspect the log\n",
         "information above for errors and make sure you can run your custom script \n",
         "on the command-line manually without any issues.\n\n",
         "The exact command that failed was:\n\n",
         cli_command, call. = FALSE)

  path
}
