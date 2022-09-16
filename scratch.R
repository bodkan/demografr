devtools::load_all(".")

devtools::load_all("~/Projects/slendr")
library(dplyr)
library(future)

plan(cluster, workers = c("r1", "r2", "r6"), homogeneous = FALSE)

p1 <- population("p1", time = 1, N = 1000)
p2 <- population("p2", time = 2000, N = 100, parent = p1)
p3 <- population("p3", time = 6000, N = 3000, parent = p2)

model <- compile_model(
  populations = list(p1, p2, p3),
  generation_time = 1,
  simulation_length = 10000,
  serialize = FALSE
)

plot_model(model)

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8) %>% ts_mutate(1e-8)

samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")

obs_diversity <- ts_diversity(ts, sample_sets = samples)$diversity

priors <- list(
  p1 ~ runif(10, 10000),
  p2 ~ runif(10, 10000),
  p3 ~ rexp(1/1000)
)

fun_diversity <- function(ts) {
  ts <- ts_mutate(ts, mutation_rate = 1e-8)
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  diversity <- ts_diversity(ts, sample_sets = samples)$diversity
  diversity
}

funs <- list(fun_diversity)

n_iters <- 1000

results <- run_abc(model, priors, funs, iterations = n_iters)

params <- results[[1]]
stats <- results[[2]]

rmse <- sqrt(rowMeans((stats - obs_diversity)^2))
best_matches <- rmse < quantile(rmse, probs = 0.05)

params[best_matches, ]







# parallel execution

library(future)

plan(cluster, workers = c("r1", "r2", "r6"), homogeneous = FALSE)






x %<-% { reticulate::py_run_string("b = 123"); reticulate::py$b }
y <- future({ population("asd", N = 123, time = 1) }, packages = "slendr")
val_y <- value(y)









pid <- Sys.getpid()

a %<-% {
  pid <- Sys.getpid()
  cat("Future 'a' ...\n")
  3.14
}

b %<-% {
  rm(pid)
  cat("Future 'b' ...\n")
  Sys.getpid()
}

c %<-% {
  cat("Future 'c' ...\n")
  2 * a
}

b
c
a
pid
