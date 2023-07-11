#' Extract names of individuals in a tree sequence
#'
#' @param ts Tree-sequence object simulated by a slendr back-end engine
#' @param split Should sample names in the tree sequence be split by a column
#'   (a population or time column)? Default is \code{NULL} and all names of
#'   samples will be returned.
#'
#' @return A vector of character sample names. If \code{split} is specified,
#'   a list of such vectors is returned, one element of the list per population
#'   or sampling time.
#'
#' @export
extract_names <- function(ts, split = NULL) {
  df <- slendr::ts_samples(ts)

  if (is.null(split)) { # return all names if splitting not requested
    result <- df$name
  } else if (split %in% colnames(df)) { # otherwise split by a given column
    result <- df %>% split(., .[[split]]) %>% lapply(`[[`, "name")
  } else
    stop("Column '", split, "' not present in the samples table", call. = FALSE)

  result
}
