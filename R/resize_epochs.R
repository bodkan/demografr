#' Plan resize events for the given population
#'
#' @param pop A slendr population object
#' @param epochs The number of resize events to schedule either at specified \code{times},
#'   or between the last encoded event in a population's history and a specified \code{until}
#'   time point
#' @param sizes A vector of population sizes (one element of the vector for each epoch)
#' @param times A vector of times of each resize event (one element of the vector for each epoch).
#'   If \code{NULL}, the times are computed by dividing the time between the last given slendr
#'   event in the population's history and the last resize time \code{until}.
#' @param until If \code{times} is not given, this argument gives the time at which the last
#'   resize event will end. See \code{times} for additional timing options.
#'
#' @export
resize_epochs <- function(pop, epochs, sizes, until = NULL, times = NULL) {
  if (epochs <= 0)
    stop("The number of resize epochs must be a non-negative integer number", call. = FASE)

  if (length(sizes) != epochs)
    stop("The vector of population sizes must be of the same length as the number of epochs", call. = FALSE)

  if (is.null(until) && is.null(times))
    stop("The end of the time window for resizes or the time of each resize must be given", call. = FALSE)

  if (!is.null(times) && length(times) != epochs)
    stop("The vector of epoch times must be of the same length as the number of epochs", call. = FALSE)

  if (is.null(until))
    until <- times[length(times)]

  # if the times of individual resize epochs are not specified, get the time of the last
  # event in the population's history, and create the time points of all resize events
  # automatically
  if (is.null(times)) {
    last_event <- attr(pop, "history") %>% .[length(.)] %>% .[[1]]
    last_time <- last_event[, intersect(c("time", "tresize"), colnames(last_event))]
    times <- round(seq(last_time, until, length.out = 1 + epochs + 1)) %>% .[-c(1, length(.))]
  }

  # generate intermediate population resize events
  for (i in seq_len(epochs)) {
    pop <- slendr::resize(pop, N = sizes[i], time = times[i], how = "step")
  }

  pop
}
