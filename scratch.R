devtools::load_all("~/Projects/slendr")
devtools::load_all(".")
library(dplyr)
library(future)

plan(cluster, workers = c("r1", "r2", "r6"), homogeneous = FALSE)
# plan(sequential)

p1 <- population("p1", time = 1, N = 1000)
p2 <- population("p2", time = 2000, N = 100, parent = p1)
p3 <- population("p3", time = 6000, N = 3000, parent = p2)

model <- compile_model(
  populations = list(p1, p2, p3),
  generation_time = 1,
  simulation_length = 10000, serialize = FALSE
)

# plot_model(model)

# generate fake "empirical" data of nucleotide diversity in each population
ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)
samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")

observed_stats <- list(
  diversity = ts_mutate(ts, mutation_rate = 1e-8) %>% ts_diversity(sample_sets = samples) %>% .$diversity
)

# setup priors
priors <- list(
  p1 ~ runif(10, 10000),
  p2 ~ runif(10, 10000),
  p3 ~ rexp(1/1000)
)

plot_priors(priors)

# setup summary statistic functions
fun_diversity <- function(ts) {
  ts <- ts_mutate(ts, mutation_rate = 1e-8)
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  result <- ts_diversity(ts, sample_sets = samples)
  result$diversity
}

funs <- list(diversity = fun_diversity)

# get ABC iterations
results <- run_abc(model, priors, funs, iterations = 100)

params <- results[[1]]
stats <- results[[2]]

rmse <- sqrt(rowMeans((stats - obs_diversity)^2))
best_matches <- rmse < quantile(rmse, probs = 0.05)

params[best_matches, ]


observed_stats <- observed_stats$diversity
names(observed_stats) <- c("diversity_1", "diversity_2", "diversity_3")

library(abc)

result <- abc(
  target = observed_stats,
  param = params,
  sumstat = stats,
  tol = 0.05,
  method = "neuralnet",
  transf = c("log")
)

result


# parallel execution

library(future)

plan(cluster, workers = c("r1", "r2", "r6"), homogeneous = FALSE)






x %<-% { reticulate::py_run_string("b = 123"); reticulate::py$b }
y <- future({ population("asd", N = 123, time = 1) }, packages = "slendr")
val_y <- value(y)
val_y


z <- future({ setup_env(); msprime(model, sequence_length = 100, recombination_rate = 0) }, packages = "slendr")
val_z <- value(z)
val_y







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
