
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *demografr*: Simple and efficient toolkit for ABC inference in R

<!-- badges: start -->
<!-- badges: end -->

The goal of the *demografr* R package is to simplify and streamline
[Approximate Bayesian
Computation](https://en.wikipedia.org/wiki/Approximate_Bayesian_computation)
(ABC) inferences in population genetics, and make ABC orders of
magnitude faster and more efficient than traditional ABC approaches by
leveraging the [tree sequences](https://tskit.dev/learn/) as an internal
data structure and computation engine.

Unlike traditional ABC inference procedues, which generally involve
custom-build pipelines and scripts for population genetic simulation and
computation of summary statistics, *demografr* makes it possible to
perform simulation, data analysis, and ABC inference itself entirely in
R within a single reproducible analysis script. By eliminating the need
to write custom simulation code and scripting for integration of various
population genetic tools for computing summary statistics, it lowers the
barrier to entry for new users and facilitates reproducibility for all
users regardless of their level of experience by eliminating many common
sources of bugs.

### What are the issues with standard ABC?

A traditional ABC analysis in population genetics involves:

1.  Writing a simulation script tailor-made for the demographic model of
    species in question using tools such as ms, scrm, msprime, or SLiM.
    For most realistic demographic models this present’s a significant
    obstacle unless one is well versed in software development.
2.  Programming a pipeline which will draw the parameters of the model
    from a set of priors, and simulate output data in an appropriate
    format. Saving outputs to disk can be very slow, especially for
    large genome-scale simulations across hundreds of thousands
    simulation replicates.
3.  Computing summary statistics on the simulated data using various
    pieces of software (ADMIXTOOLS, PLINK, vcftools, or even custom
    scripts), often requiring conversion of the simulated outputs in an
    appropriate file format required by each software. In addition to
    the cost of disk-access, computation of summary statistics itself
    can be quite slow depending on the statistic or program in question.
4.  Feeding (and often re-formatting) the outputs of summary statistics
    in the right format to use with the appropriate ABC inference
    engine, such as the R package
    [*abc*](https://cran.r-project.org/package=abc).

Given the range of disparate tools required for the steps above,
traditional ABC pipelines end up being quite complex, require a large
amount of programming work, and due the unique characteristics of
application need to be often re-invented from scratch for each study.

All in all, an unnecessary amount of time spent on ABC analyses is
dedicated to software development—programming, debugging, and data
munging—work that would be better spent doing research.

### How does *demografr* help?

*demografr* streamlines every step of a typical ABC pipeline by
leveraging the [*slendr*](https://github.com/bodkan/slendr/) framework
as a building block for simulation and data analysis, making it possible
to perform an entire ABC analysis in R. Taking the steps above one by
one:

1.  *slendr*’s intuitive functionality for definning population genetic
    models makes it easy to encode even complex demographic models with
    only bare minimum of R knowledge needed.
2.  *demografr* makes it possible to encode prior distributions of
    parameters using familiar R interface which resembles standard
    probabilistic statements, and provides an automated function which
    simulates a defined number of ABC simulation replicates in a trivial
    manner.
3.  Because *slendr*’s simulation output is a [tree
    sequence](https://tskit.dev/learn/), most population genetic
    statistics can be computed directly on such tree sequences using R
    functions which are part of *slendr*’s statistical library. Tree
    sequence is never saved to disk and no conversion between file
    formats is required.
4.  *demografr* facilitates tight integration with the powerful R
    package [*abc*](https://cran.r-project.org/package=abc) by
    automatically feeding ABC simulation data to the *abc* package for
    inference.

## Installation

You can install the development version of *demografr* from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("bodkan/demografr")
```

Note that this requires an R package *devtools*, which you can obtain
simply by running `install.packages("devtools")`.

## Example

Imagine that we sequenced genomes of individuals from three populations
“p1”, “p2”, and “p3”.

Let’s also assume that we know that the three populations are related in
the following way but we don’t know anything else (i.e., we have no idea
about the effective population sizes $N_e$ or split times). For
simplicity, how can we use ABC to get the posterior distributions of
$N_e$ for each population from some summary statistics we computed from
the “real data” (maybe a VCF).

    #> The interface to all required Python modules has been activated.

<img src="man/figures/README-model_graph-1.png" width="100%" />
