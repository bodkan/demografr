devtools::load_all(".")
library(slendr)
library(dplyr)
library(future)
library(abc)

nodes <- c("r1", "r2", "r3", "r4", "r5", "r6")
plan(list(tweak(cluster, workers = nodes, homogeneous = FALSE),
          tweak(multisession, workers = 50)))
plan(multicore, workers = parallel::detectCores())

p1 <- population("p1", time = 1, N = 1000)
p2 <- population("p2", time = 2000, N = 100, parent = p1)
p3 <- population("p3", time = 6000, N = 3000, parent = p2)

model <- compile_model(
  populations = list(p1, p2, p3),
  generation_time = 1,
  simulation_length = 10000, serialize = FALSE
)

plot_model(model)

# generate fake "empirical" data of nucleotide diversity in each population
ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)
samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")

observed_stats <- list(
  pi = ts_mutate(ts, mutation_rate = 1e-8) %>% ts_diversity(sample_sets = samples) %>% mutate(stat = paste0("pi_", set), value = diversity) %>% dplyr::select(stat, value)
)

# setup priors
priors <- list(
  p1 ~ runif(10, 10000),
  p2 ~ runif(10, 10000),
  p3 ~ runif(10, 10000)
)

plot_priors(priors)

# setup summary statistic functions
compute_diversity <- function(ts) {
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  ts_diversity(ts, sample_sets = samples) %>%
    mutate(stat = paste0("pi_", set)) %>%
    dplyr::select(stat, value = diversity)
}

functions <- list(pi = compute_diversity)

# get ABC iterations
data <- simulate_abc(
  model, priors,
  summary_funs = functions,
  observed_stats = observed_stats,
  iterations = 10000, mutation_rate = 1e-8
)
saveRDS(data, "/tmp/data.rds")

result <- perform_abc(data, tolerance = 0.05, method = "neuralnet")

attr(result, "parameters") <- data$parameters
class(result) <- c("demographr_abc", "abc")

# yay it works!
summary(result)

plot_model(model)

hist(result, breaks = 50)
plot(result, param = data$parameters)


hist.demographr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    x$names$parameter.names <- param
    params <- params[, param]
  }
  abc:::hist.abc(x, ...)
}

plot.demographr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    params <- params[, param]
  }
  abc:::plot.abc(x, param = params, ...)
}

result$adj.values %>%
  as_tibble() %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(value, color = variable)) +
  geom_density() +
  geom_vline(xintercept = summary(result)[4, ])

# parallel execution
#
# library(future)
#
# plan(cluster, workers = c("r1", "r2", "r3", "r4", "r5", "r6"), homogeneous = FALSE)
#
#
# x %<-% { reticulate::py_run_string("b = 123"); reticulate::py$b }
# y <- future({ population("asd", N = 123, time = 1) }, packages = "slendr")
# val_y <- value(y)
# val_y
#
#
# z <- future({
#   setup_env()
#   ts <- msprime(model, sequence_length = 100, recombination_rate = 0)
#   ts_diversity(ts, sample_sets = c("p1_1", "p1_2"), mode = "branch")
# }, packages = "slendr")
# val_z <- value(z)
# val_z
#
#
#
#
#
#
#
# pid <- Sys.getpid()
#
# a %<-% {
#   pid <- Sys.getpid()
#   cat("Future 'a' ...\n")
#   3.14
# }
#
# b %<-% {
#   rm(pid)
#   cat("Future 'b' ...\n")
#   Sys.getpid()
# }
#
# c %<-% {
#   cat("Future 'c' ...\n")
#   2 * a
# }
#
# b
# c
# a
# pid
