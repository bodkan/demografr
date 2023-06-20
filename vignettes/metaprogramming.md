How can we let the user define probabilistic statements in R and have
the code be as close as possible to normal mathematical notation?

I.e., things like:

$$N_e \sim Unif(1000, 10000)$$

$$\textrm{geneflow} \sim Unif(0, 1)$$

$$T_{\textrm{split}}^i \sim Norm(10, 10000), \textrm{for } i \in \{1, 2, ..., 10\}$

## One trivial option

Something like this is used by EasyABC R package:

``` r
list(
  list("Ne", "runif", 1000, 10000),
  list("geneflow", "runif", 0, 1),
  # (not sure how would one create a vector like T_i)
)
```

In Python, you might create some objects like:

``` python
import tensorflow_probability as tfp

# define random variables
Ne = tfp.distributions.Uniform(low=1000., high=10000.)
geneflow = tfp.distributions.Uniform(low=0., high=1.)
T_split = tfp.distributions.Uniform(low=10., high=10000.)

# sample from the distributions
Ne_sample = Ne.sample()
geneflow_sample = geneflow.sample()
T_split_sample = T_split.sample(10)
```

**Can we do this more “elegantly” with less code and closer to actual
mathematical notation?**

## Fancier metaprogramming solution

Despite being *“a stupid calculator language”* (quote by Graham), R is a
very powerful functional programming language with advanced
metaprogramming options.

> Metaprogramming is a **programming technique in which computer
> programs have the ability to treat other programs as their data**. It
> means that a program can be designed to read, generate, analyze or
> transform other programs, and even modify itself while running.
>
> In other words, **metaprogramming is when code is writing other
> code.**

You might be familiar with the syntax for fitting linear models in R:

``` r
head(mtcars)
```

    #>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    #> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    #> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    #> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    #> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    #> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    #> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r
lm(cyl ~ mpg, data = mtcars)
```

    #>
    #> Call:
    #> lm(formula = cyl ~ mpg, data = mtcars)
    #>
    #> Coefficients:
    #> (Intercept)          mpg
    #>     11.2607      -0.2525

The **`~` operator creates a so-called “formula object”**, which
captures the expression provided by the user, and can then be
manipulated or interpreted by the function that the formula is passed
to.

## How to use this for the problem above?

Imagine then that we want to write an R interface for doing something
like this:

\[N_e \sim Unif(1000, 10000).\]

Obviously, we have functions such as `runif`, `rnorm`, etc., which
generate a number from a distribution:

``` r
runif(n = 1, min = 1000, max = 10000)
```

    #> [1] 5957.106

We can also do this in R, “for free”, without doing anything else.

``` r
prior <- Ne ~ runif(1000, 10000)

prior
```

    #> Ne ~ runif(1000, 10000)

**This expression is not evaluated\! Formula is simply an object
carrying a bit of syntax.**

This is important, because the `runif(1000, 10000)` on the right is not
a valid R code.

**This is called “lazy evaluation”.** R does not evaluate something at
the time unless it’s “needed” at that time. The opposite to this is
“eager evaluation” done by something like Python or C.

## Metaprogramming in R

Metaprogramming is often magic requiring lots of complicated tools, but
to do very basic metaprogramming in R is very simple.

How can we “manipulate” code and give it our own meaning?

``` r
prior
```

    #> Ne ~ runif(1000, 10000)

When given a formula, the R function **`as.list` parses the language
expression into smallest possible “atomic pieces”**:

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #>
    #> [[2]]
    #> Ne
    #>
    #> [[3]]
    #> runif(1000, 10000)

This is called an **Abstract Syntax Tree** in programming language
design.

Note that this works generally for any language expression (even those
that are much deeper):

``` r
(a + b) * (c - d / e)
```

But for expressions like the above it is easier to use a proper
metaprogramming tool:

``` r
lobstr::ast((a + b) * (c - d / e))
```

    #> █─`*`
    #> ├─█─`(`
    #> │ └─█─`+`
    #> │   ├─a
    #> │   └─b
    #> └─█─`(`
    #>   └─█─`-`
    #>     ├─c
    #>     └─█─`/`
    #>       ├─d
    #>       └─e

## Back to *demografr*

Coming back to our “prior sampling statement” example:

``` r
prior
```

    #> Ne ~ runif(1000, 10000)

which we can parse like this:

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #>
    #> [[2]]
    #> Ne
    #>
    #> [[3]]
    #> runif(1000, 10000)

And which we hope to interpret in a way that’s compatible with something
like \(N_e \sim Unif(1000, 10000)\).

*demografr* parses each such prior sampling statement, and automatically
converts the “invalid” expression:

``` r
Ne ~ runif(1000, 10000)
```

into a proper R statement

``` r
runif(n = 1, min = 1000, max = 10000)
```

which is then used during the ABC simulation process.

### `sample_prior()` function

This is implemented by an (internal) *demografr* function
`sample_prior`:

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #>
    #> $value
    #> [1] 8724.988

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #>
    #> $value
    #> [1] 6651.011

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #>
    #> $value
    #> [1] 5318.861

## Sampling random vectors

To provide support for random vectors, *demografr* uses a similar
metaprogramming trick as above.

Imagine this syntax for vectorized statements (the `T_split[10]` is my
made up syntax, not R syntax\!):

``` r
prior <- T_split[10] ~ runif(10, 10000)
```

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #>
    #> [[2]]
    #> T_split[10]
    #>
    #> [[3]]
    #> runif(10, 10000)

Note that this expression is one layer “deeper” than would be with a
plain `T_split ~ runif(10, 10000)`.

This means that for “vector variables” of *demografr*, we need to take
care of the nesting:

``` r
variable <- as.list(prior)[[2]]
variable
```

    #> T_split[10]

``` r
as.list(variable)
```

    #> [[1]]
    #> `[`
    #>
    #> [[2]]
    #> T_split
    #>
    #> [[3]]
    #> [1] 10

### In *demografr* then…

``` r
sample_prior(T_split[10] ~ runif(10, 10000))
```

    #> $variable
    #> [1] "T_split"
    #>
    #> $value
    #>  [1]  519.4349 2090.2553 6629.0542 5651.4096  255.6859 4876.1447 7336.7094
    #>  [8] 2107.1281 7924.6542 4743.1582

Which internally turns the prior expression into `runif(n = 10, min
= 10, max = 10000)`.

## A list of all priors

``` r
priors <- list(
  Ne          ~ runif(1000, 1000),
  geneflow    ~ runif(0, 1),
  T_split[10] ~ runif(10, 1000)
)
```

**The point of this is that this is *declarative*\! It defines the
priors, but doesn’t sample any numbers\!**

But, during each ABC replicate simulation, *demografr* then internally
performs the equivalent of this:

``` r
lapply(priors, sample_prior) # iterate over the list of priors, sample from each
```

    #> [[1]]
    #> [[1]]$variable
    #> [1] "Ne"
    #>
    #> [[1]]$value
    #> [1] 1000
    #>
    #>
    #> [[2]]
    #> [[2]]$variable
    #> [1] "geneflow"
    #>
    #> [[2]]$value
    #> [1] 0.3172069
    #>
    #>
    #> [[3]]
    #> [[3]]$variable
    #> [1] "T_split"
    #>
    #> [[3]]$value
    #>  [1] 662.92317 984.27174 858.11574  31.45234 912.07427 739.95854 932.88907
    #>  [8] 752.20514 586.05837  18.86225

**But this is something the user never has to do or care about**, it is
done automatically and internally when they call:

``` r
data <- simulate_abc(model, <priors>, functions, observed, ...)
```

## Custom functions

The above also allows custom-defined prior distribution functions, not
just built-in `runif`, `rnorm`, etc.

For instance, imagine this function simulating a loaded casino die:

``` r
casino_d6 <- function(n = 1) {
  outcomes      <- c(1, 2, 3, 4, 5, 6) # six sides of a die
  probabilities <- c(1, 1, 1, 1, 1, 3) # rolling a 6 is 3x more likely!

  sample(outcomes, size = n, prob = probabilities, replace = TRUE)
}
```

``` r
prior <- rolls[100] ~ casino_d6()

samples <- sample_prior(prior)
samples
```

    #> $variable
    #> [1] "rolls"
    #>
    #> $value
    #>   [1] 6 6 1 1 3 3 4 6 2 2 4 1 4 2 2 1 6 6 1 6 1 1 6 5 4 6 1 6 1 6 3 2 6 6 6 6 1
    #>  [38] 2 4 6 6 6 1 3 2 4 5 6 3 6 2 3 5 5 1 1 2 6 6 3 1 3 1 1 2 5 5 6 4 6 4 4 5 1
    #>  [75] 3 5 1 6 4 4 4 2 2 5 3 2 5 4 5 6 4 2 2 6 6 3 2 5 1 4

``` r
table(samples$value)
```

    #>
    #>  1  2  3  4  5  6
    #> 19 16 11 15 12 27
