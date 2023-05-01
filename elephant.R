#!/usr/bin/env Rscript

devtools::load_all(".")
library(slendr)
library(dplyr)
library(readr)

# set up the internal tskit/msprime environment
init_env()

# set up parallelization across 80 CPUs
library(future)
plan("multicore", workers = availableCores())

#---------------------------------------------------------------------
# bind data frames with empirical summary statistics into a named list
observed_diversity <- read_tsv("formatted_diversity.tsv")
observed <- list(
  diversity  = observed_diversity #,
#   divergence = observed_divergence,
#   f4         = observed_f4
)

#--------------------------------------------------------------------------------
# define a model generating function using the slendr interface
# (each of the function parameters correspond to a parameter we want to infer)

#   T_ELE_Mame <- 7e6 # 12.5 - 2.9
#   T_EM_LP <- 4.8e6 # 4.8 - 1.3
#   T_LOX_Pant <- 3e6 # 5.2 - 2.1
#   T_Emax_MAM <- 2e6 # 2.8 - 0.6
#   T_Lafr_Lcyc <- 2e6 # 5.6 - 2.6
#   gf_Lcyc_Pant <-  0.25
#   gf_EM_Pant <-  0.45
#   Ne_ANC <-  100e3 # 2,000 - 160,000
#   Ne_ELE <- 50e3 # 7,000 - 78,000
#   Ne_LP <- 60e3 # 37,000 - 233,000
#   Ne_LOX <- 150e3 # 29,000 - 175,000
#   Ne_EM <- 60e3 # 10,000 - 130,000
#   T_ANC <- 15e6 # 28.4 - 10.4

model <- function(Ne_Mame, Ne_Lafr, Ne_Lcyc, Ne_Pant, Ne_Emax, Ne_MAM,
				T_ELE_Mame = 7e6, T_EM_LP = 4.8e6, T_LOX_Pant = 3e6, T_Emax_MAM = 2e6, T_Lafr_Lcyc = 2e6,
				gf_Lcyc_Pant =  0.25, gf_EM_Pant = 0.45, Ne_ANC = 100e3, Ne_ELE = 50e3, Ne_LP = 60e3, Ne_LOX = 150e3,
				Ne_EM = 60e3, T_ANC = 15e6) {
  # Ancestor populations
  popANC <- population("Ancestor", time = T_ANC, N = Ne_ANC, remove = T_ELE_Mame)
  popELE <- population("Anc_Elephantids", time = T_ELE_Mame, N = Ne_ELE, remove = T_EM_LP, parent = popANC)
  popLP <- population("Anc_Lox_Prim", time = T_EM_LP, N = Ne_LP, remove = T_LOX_Pant, parent = popELE)
  popLOX <- population("Anc_Loxodonta", time = T_LOX_Pant, N = Ne_LOX, remove = T_Lafr_Lcyc, parent = popLP)
  popEM <- population("Anc_Ele_Mam", time = T_EM_LP, N = Ne_EM, remove = T_Emax_MAM, parent = popELE)

  # Extant populations
  popMame <- population("M_americanum", time = T_ELE_Mame,	N = Ne_Mame, parent = popANC)
  popLafr <- population("L_africana", 	time = T_Lafr_Lcyc, N = Ne_Lafr, parent = popLOX)
  popLcyc <- population("L_cyclotis", 	time = T_Lafr_Lcyc, N = Ne_Lcyc, parent = popLOX)
  popPant <- population("P_antiquus", 	time = T_LOX_Pant, 	N = Ne_Pant, parent = popLP)
  popEmax <- population("E_maximus", 	time = T_Emax_MAM, 	N = Ne_Emax, parent = popEM)
  popMAM <- population("Mammoths", 		time = T_Emax_MAM, 	N = Ne_MAM, parent = popEM)


  gf <- list(gene_flow(from = popLcyc, to = popPant, start = 1e6, end = (1e6-31), rate = gf_Lcyc_Pant),
  			 gene_flow(from = popEM, to = popPant, start = 2.5e6, end = (2.5e6-31), rate = gf_EM_Pant))

  model <- compile_model(
    populations = list(popANC, popELE, popLP, popLOX, popEM, popMame,
                       popLafr, popLcyc, popPant, popEmax, popMAM),
	  gene_flow = gf,
    generation_time = 31,
    serialize = FALSE
  )

  samples <- schedule_sampling(
    model, times = 0,
    list(popEmax, 2), list(popLafr, 2), list(popLcyc, 2), list(popMame, 2), list(popMAM, 2), list(popPant, 2)
  )

  return(list(model, samples))
}

#--------------------------------------------------------------------------------
# setup priors for model parameters
# Page 106 and 107 in the supplement
# Ne Uniform: [1000-500,000]
# Ne12 Uniform: [0.01-2]* Ne
# Ne123 Uniform: [0.01-2]* Ne
# Ne1234 Uniform: [0.01-2]* Ne
# t1 Uniform: [1,000,000-10,000,000]
# t2 ~ Uniform: [1.01-5]*t1
# t3 ~ Uniform: [1.01-5]* t2
# mA-B ~ Exponential: [λ = 2×10-7]
# mA-C ~ Exponential: [λ = 2×10-7]
# mB-C ~ Exponential: [λ = 2×10-7]
# % recur ~ Uniform: [0-0.1]
# μ ~ Fixed: 0.406×10-9
# r ~ Fixed: 1×10-8
# gen time ~ Fixed: 31


priors <- list(
#   Ne_A ~ runif(100, 10000),
#   Ne_B ~ runif(100, 10000),
#   Ne_C ~ runif(100, 10000),
#   Ne_D ~ runif(100, 10000),

#   T_AB ~ runif(1, 3000),
#   T_BC ~ runif(3000, 6000),
#   T_CD ~ runif(6000, 9000),

#   gf_BC ~ runif(0, 1),

  Ne_Mame ~ runif(1e3, 500e3),
  Ne_Lafr ~ runif(1e3, 500e3),
  Ne_Lcyc ~ runif(1e3, 500e3),
  Ne_Pant ~ runif(1e3, 500e3),
  Ne_Emax ~ runif(1e3, 500e3),
  Ne_MAM ~ runif(1e3, 500e3) # 8,000 - 30,000

#   Ne_Mame ~ runif(1e2, 1e3),
#   Ne_Lafr ~ runif(1e2, 1e3),
#   Ne_Lcyc ~ runif(1e2, 1e3),
#   Ne_Pant ~ runif(1e2, 1e3),
#   Ne_Emax ~ runif(1e2, 1e3),
#   Ne_MAM ~ runif(1e2, 1e3)

#   T_ELE_Mame <- 7e6 # 12.5 - 2.9
#   T_EM_LP <- 4.8e6 # 4.8 - 1.3
#   T_LOX_Pant <- 3e6 # 5.2 - 2.1
#   T_Emax_MAM <- 2e6 # 2.8 - 0.6
#   T_Lafr_Lcyc <- 2e6 # 5.6 - 2.6
#   gf_Lcyc_Pant <-  0.25
#   gf_EM_Pant <-  0.45
#   Ne_ANC <-  100e3 # 2,000 - 160,000
#   Ne_ELE <- 50e3 # 7,000 - 78,000
#   Ne_LP <- 60e3 # 37,000 - 233,000
#   Ne_LOX <- 150e3 # 29,000 - 175,000
#   Ne_EM <- 60e3 # 10,000 - 130,000
#   T_ANC <- 15e6 # 28.4 - 10.4
)

#--------------------------------------------------------------------------------
# define summary functions to be computed on simulated data (must be of the
# same format as the summary statistics computed on empirical data)
compute_diversity <- function(ts) {
  species <- c("E_maximus", "L_africana", "L_cyclotis", "M_americanum", "Mammoths", "P_antiquus")

  samples <- ts_samples(ts) %>%
    dplyr::filter(pop %in% species) %>%
    split(., .$pop) %>%
    lapply(function(df) sample(df$name, 2))

  div <- ts_diversity(ts, sample_sets = samples) %>% dplyr::arrange(set)

  div
}
compute_divergence <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_divergence(ts, sample_sets = samples)
}
compute_f4 <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_f4(ts,
        W = list(popA = samples$popA),
        X = list(popB = samples$popB),
        Y = list(popC = samples$popC),
        Z = list(popD = samples$popD))
}
# the summary functions must be also bound to an R list named in the same
# way as the empirical summary statistics
functions <- list(
  diversity  = compute_diversity#,
#   divergence = compute_divergence,
#   f4         = compute_f4
)

#--------------------------------------------------------------------------
# sanity check model by evaluating model statistics
# Ne_Mame <- 100e3
# Ne_Lafr <- 150e3
# Ne_Lcyc <- 150e3
# Ne_Pant <- 50e3
# Ne_Emax <- 60e3
# Ne_MAM <- 20e3 # 8,000 - 30,000
# T_ELE_Mame <- 7e6 # 12.5 - 2.9
# T_EM_LP <- 4.8e6 # 4.8 - 1.3
# T_LOX_Pant <- 3e6 # 5.2 - 2.1
# T_Emax_MAM <- 2e6 # 2.8 - 0.6
# T_Lafr_Lcyc <- 2e6 # 5.6 - 2.6
# gf_Lcyc_Pant <-  0.25
# gf_EM_Pant <-  0.45
# Ne_ANC <-  100e3 # 2,000 - 160,000
# Ne_ELE <- 50e3 # 7,000 - 78,000
# Ne_LP <- 60e3 # 37,000 - 233,000
# Ne_LOX <- 150e3 # 29,000 - 175,000
# Ne_EM <- 60e3 # 10,000 - 130,000
# T_ANC <- 15e6 # 28.4 - 10.4

# test_model <- model(Ne_Mame, Ne_Lafr, Ne_Lcyc, Ne_Pant, Ne_Emax, Ne_MAM,
# 				T_ELE_Mame, T_EM_LP, T_LOX_Pant, T_Emax_MAM, T_Lafr_Lcyc,
# 				gf_Lcyc_Pant, gf_EM_Pant, Ne_ANC, Ne_ELE, Ne_LP, Ne_LOX,
# 				Ne_EM, T_ANC)

# ts <- msprime(test_model, sequence_length = 1e6, recombination_rate = 10e-8)
# # recombination rate and mutation rate taken from page 105 of supplement
# ts <- ts_mutate(ts, mutation_rate = 0.406e-9)
# test_div <- compute_divergence(ts) %>%
# 	arrange(divergence)



#--------------------------------------------------------------------------------
# validate the individual ABC components for correctness and consistency
validate_abc(model, priors, functions, observed)

# Easy way to look at calculated summary stats
ts <- simulate_ts(model, priors, recombination_rate = 1e-8, mutation_rate = 1e-8)

#--------------------------------------------------------------------------------
# run ABC simulations
data <- simulate_abc(
  model, priors, functions, observed, iterations = 1000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

#--------------------------------------------------------------------------------
# infer posterior distributions of parameters
# (accepts all parameters of the abc() function from the R package abc)
abc <- perform_abc(data, tolerance = 0.05, method = "neuralnet")