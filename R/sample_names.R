#' Extract names of individuals in a tree sequence
#'
#' @param ts Tree-sequence object simulated by a slendr back-end engine
#' @param split Should sample names be split by population? Default is \code{FALSE}
#'
#' @return A vector of character sample names. If \code{split} is \code{TRUE},
#'   a list of such vectors is returned, one element of the list per population.
#'
#' @export
sample_names <- function(ts, split = FALSE) {
  if (split)
    slendr::ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  else
    slendr::ts_samples(ts)$name
}
