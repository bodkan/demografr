
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *demografr*: A simple and efficient ABC toolkit for R

<!-- badges: start -->

<!-- badges: end -->

<img src="man/figures/logo.png" align="right" />

⚠️⚠️⚠️

**This package is under active development and things often change (and
break) on short notice\! You probably shouldn’t be using *demografr* in
your own modeling projects at this point.**

**A more stable version and a preprint will be ready sometime in summer
2023. Feedback is most welcome\!**

⚠️⚠️⚠️

The goal of *demografr* is to simplify and streamline [Approximate
Bayesian
Computation](https://en.wikipedia.org/wiki/Approximate_Bayesian_computation)
(ABC) in population genetics and make it more reproducible.
Additionally, *demografr* aims to make ABC orders of magnitude faster
and more efficient by leveraging the [tree
sequences](https://tskit.dev/learn/) as an internal data structure and
computation engine.

Unlike traditional ABC approaches, which generally involve custom-built
pipelines and scripts for population genetic simulation and computation
of summary statistics, *demografr* makes it possible to perform
simulation, data analysis, and ABC inference itself entirely in R within
a single reproducible analysis script. By eliminating the need to write
custom simulation code and scripting for integration of various
population genetic tools for computing summary statistics, it lowers the
barrier to entry for new users and facilitates reproducibility for all
users regardless of their level of experience by eliminating many common
sources of bugs.

### How does *demografr* help with ABC?

*demografr* streamlines every step of a typical ABC pipeline by
leveraging the [*slendr*](https://github.com/bodkan/slendr/) framework
as a building block for simulation and data analysis, making it possible
to write complete ABC workflows in R. Specifically:

1.  *slendr*’s intuitive, interactive [interface for definning
    population genetic
    models](https://www.slendr.net/articles/vignette-04-nonspatial-models.html)
    makes it easy to encode even complex demographic models with only
    bare minimum of R knowledge needed.
2.  *demografr* makes it possible to encode prior distributions of
    parameters using familiar R interface resembling standard
    probabilistic statements, and provides an automated function which
    simulates ABC replicates drawing parameters from priors in a
    trivial, one-step manner.
3.  Because *slendr*’s simulation output is a [tree
    sequence](https://tskit.dev/learn/), most population genetic
    statistics can be computed directly on such tree sequences using R
    functions which are part of *slendr*’s statistical library. A tree
    sequence is never saved to disk and no conversion between file
    formats is required.
4.  *demografr* facilitates tight integration with the powerful R
    package [*abc*](https://cran.r-project.org/package=abc) by
    automatically feeding its simulation data to the *abc* package for
    inference and analysis.

## Installation

You can install the development version of *demografr* from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("bodkan/demografr")
```

Note that this requires an R package *devtools*, which you can obtain
simply by running `install.packages("devtools")`.

Because *demografr* is tightly linked to the *slendr* simulation package
(in fact, new developments in *slendr* ale currently driven by
requirements of *demografr*), you will also need the development version
of *slendr* itself:

``` r
devtools::install_github("bodkan/slendr")
```

### Note on stability

*demografr* is very much in an early experimental stage at this point.
Although ABC fitting of “standard” demographic models (i.e. estimating
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e"), split times and gene-flow parameters for non-spatial models)
already works very nicely, our long-term ambitions for the project are
much higher. As such, please be aware that the interface might change
significantly on a short notice to accomodate features for estimating
parameters of more complex custom models such as spatial models etc.

If you want to follow updates on *demografr*, you can do this also on my
[Twitter](https://twitter.com/fleventy5). I am not very active there but
I do use it to post notes about all my software projects.

## Important pieces missing so far

Currently in progress:

1.  Support for temporal sampling via *slendr*’s
    [schedule\_sampling()](https://www.slendr.net/reference/schedule_sampling.html).

2.  Implement flexible time units for model parameters as supported by
    *slendr* (years ago, generations forwards in time, years into the
    future, etc.).

3.  Implement rejection of non-sensical parameter combinations (daughter
    populations existing before parent populations, etc.). Easy to solve
    internally in `simulate_abc()`, it just hasn’t happened yet.

## An example ABC analysis

Imagine that we sequenced genomes of individuals from populations
“popA”, “popB”, “popC”, and “popD”.

Let’s also assume that we know that the three populations are
phylogenetically related in the following way with an indicated
gene-flow event at a certain time in the past, but we don’t know
anything else (i.e., we have no idea about their
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") or split times):

<img src="man/figures/README-ape_tree-1.png" style="display: block; margin: auto;" />

After sequencing the genomes of individuals from these populations, we
computed the nucleotide diversity in these populations as well as their
pairwise genetic divergence, and observed the following values which we
saved in two standard R data frames:

1.  Nucleotide diversity in each population:

<!-- end list -->

``` r
observed_diversity <- read.table(system.file("examples/observed_diversity.tsv", package = "demografr"), header = TRUE)

observed_diversity
#>    set    diversity
#> 1 popA 8.079807e-05
#> 2 popB 3.324979e-05
#> 3 popC 1.024510e-04
#> 4 popD 9.024937e-05
```

2.  Pairwise divergence d\_X\_Y between populations X and Y:

<!-- end list -->

``` r
observed_divergence <- read.table(system.file("examples/observed_divergence.tsv", package = "demografr"), header = TRUE)

observed_divergence
#>      x    y   divergence
#> 1 popA popB 0.0002413010
#> 2 popA popC 0.0002409678
#> 3 popA popD 0.0002407488
#> 4 popB popC 0.0001114809
#> 5 popB popD 0.0001151775
#> 6 popC popD 0.0001114729
```

3.  Value of the following
    ![f\_4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f_4
    "f_4")-statistic:

<!-- end list -->

``` r
observed_f4  <- read.table(system.file("examples/observed_f4.tsv", package = "demografr"), header = TRUE)

observed_f4
#>      W    X    Y    Z            f4
#> 1 popA popB popC popD -1.959654e-06
```

### A complete ABC analysis in a single R script

This is how we would use *demografr* to estimate the
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") and split times for all populations (and the rate of the
indicated gene-flow event) with Approximate Bayesian Computation in a
single R script:

``` r
library(demografr)
library(slendr)

# set up the internal tskit/msprime environment
init_env()

# set up parallelization across all CPUs
library(future)
plan(multicore, workers = availableCores())

#--------------------------------------------------------------------------------
# bind data frames with empirical summary statistics into a named list
observed <- list(
  diversity  = observed_diversity,
  divergence = observed_divergence,
  f4         = observed_f4
)

#--------------------------------------------------------------------------------
# define a model generating function using the slendr interface
# (each of the function parameters correspond to a parameter we want to infer)
model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_AB, T_BC, T_CD, gf_BC = 0.5) {
  popA <- population("popA", time = 1,    N = Ne_A)
  popB <- population("popB", time = T_AB, N = Ne_B, parent = popA)
  popC <- population("popC", time = T_BC, N = Ne_C, parent = popB)
  popD <- population("popD", time = T_CD, N = Ne_D, parent = popC)

  gf <- gene_flow(from = popB, to = popC, start = 9000, end = 9301, rate = gf_BC)

  model <- compile_model(
    populations = list(popA, popB, popC, popD), gene_flow = gf,
    generation_time = 1, simulation_length = 10000,
    direction = "forward", serialize = FALSE
  )

  samples <- schedule_sampling(
    model, times = 10000,
    list(popA, 2), list(popB, 2), list(popC, 2), list(popD, 2),
    strict = TRUE
  )

  # a return statement is mandatory!
  # if a sampling schedule is not generated, use return(model)
  return(list(model, samples))
}

#--------------------------------------------------------------------------------
# setup priors for model parameters
priors <- list(
  Ne_A ~ runif(1, 10000),
  Ne_B ~ runif(1, 10000),
  Ne_C ~ runif(1, 10000),
  Ne_D ~ runif(1, 10000),

  T_AB ~ runif(1, 10000),
  T_BC ~ runif(1, 10000),
  T_CD ~ runif(1, 10000)
)

#--------------------------------------------------------------------------------
# define summary functions to be computed on simulated data (must be of the
# same format as the summary statistics computed on empirical data)
compute_diversity <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_diversity(ts, sample_sets = samples)
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
  diversity  = compute_diversity,
  divergence = compute_divergence,
  f4         = compute_f4
)

#--------------------------------------------------------------------------------
# validate the individual ABC components for correctness and consistency
validate_abc(model, priors, functions, observed)

simulate_ts(model, priors)

#--------------------------------------------------------------------------------
# run ABC simulations
data <- simulate_abc(
  model, priors, functions, observed, iterations = 10000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

#--------------------------------------------------------------------------------
# infer posterior distributions of parameters using the abc R package
abc <- perform_abc(data, engine = "abc", tol = 0.03, method = "neuralnet")
```

## Analysing posterior distributions of parameters

After we run this R script, we end up with an object called `abc` here.
This object contains the complete information about the results of our
inference. In particular, it carries the posterior samples for our
parameters of interest
(![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") of populations and their split times).

For instance, we can get a table of all posterior values with the
function `extract_summary()`:

``` r
extract_summary(abc)
#>                             Ne_A      Ne_B       Ne_C       Ne_D      T_AB
#> Min.:                   131.0602  326.5349   419.6357 -1240.4912  981.1159
#> Weighted 2.5 % Perc.:   654.2305  564.8395  1735.3387   543.2424 1321.4739
#> Weighted Median:       1789.7170  883.1900  5210.7704  2721.9058 1961.5485
#> Weighted Mean:         1773.0775  933.2370  5424.4416  2884.5651 1938.9777
#> Weighted Mode:         1419.8437  764.8161  4641.8267  2416.2203 2001.3811
#> Weighted 97.5 % Perc.: 3113.7142 1291.6402  9652.2323  5435.2615 2554.0757
#> Max.:                  3747.5379 1683.1578 12599.8615  6726.5713 3068.6103
#>                            T_BC     T_CD       gf_BC
#> Min.:                  1709.853 2983.732 -0.11000244
#> Weighted 2.5 % Perc.:  3315.077 6825.789  0.02278014
#> Weighted Median:       5191.074 8338.294  0.24766471
#> Weighted Mean:         5094.215 8266.331  0.27152584
#> Weighted Mode:         5402.882 8424.407  0.16155855
#> Weighted 97.5 % Perc.: 6114.467 9334.816  0.66591385
#> Max.:                  6362.867 9875.401  0.80173911
```

We can also specify a subset of model parameters to select, or provide a
regular expression for this subsetting:

``` r
extract_summary(abc, param = "Ne")
#>                             Ne_A      Ne_B       Ne_C       Ne_D
#> Min.:                   131.0602  326.5349   419.6357 -1240.4912
#> Weighted 2.5 % Perc.:   654.2305  564.8395  1735.3387   543.2424
#> Weighted Median:       1789.7170  883.1900  5210.7704  2721.9058
#> Weighted Mean:         1773.0775  933.2370  5424.4416  2884.5651
#> Weighted Mode:         1419.8437  764.8161  4641.8267  2416.2203
#> Weighted 97.5 % Perc.: 3113.7142 1291.6402  9652.2323  5435.2615
#> Max.:                  3747.5379 1683.1578 12599.8615  6726.5713
```

We can also visualize the posterior distributions. Rather than plotting
many different distributions at once, let’s first check out the
posterior distributions of inferred
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") values:

``` r
plot_posterior(abc, param = "Ne")
```

![](man/figures/README-posterior_Ne-1.png)<!-- -->

Similarly, we can take a look at the inferred posteriors of the split
times:

``` r
plot_posterior(abc, param = "T")
```

![](man/figures/README-posterior_Tsplit-1.png)<!-- -->

And, finally, the rate of gene flow:

``` r
plot_posterior(abc, param = "gf")
```

![](man/figures/README-posterior_gf-1.png)<!-- -->

Finally, we have the diagnostic functionality of the
[*abc*](https://cran.r-project.org/web/packages/abc/vignettes/abcvignette.pdf)
R package at our disposal:

``` r
plot(abc, param = "Ne_C")
```

![](man/figures/README-diagnostic_Ne-1.png)<!-- -->

## Additional functionality

*demografr* also provides a couple of functions designed to make
troubleshooting a little easier.

For instance, assuming we have `priors` set up as above, we can
visualize the prior distribution(s) like this:

``` r
plot_prior(priors, "Ne")
```

![](man/figures/README-prior_Ne-1.png)<!-- -->

``` r
plot_prior(priors, c("^T", "^gf"), facet = TRUE)
```

![](man/figures/README-prior_T_gf-1.png)<!-- -->
