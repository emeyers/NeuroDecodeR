
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NDTr: The Neural Decoding Toolbox in R

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/emeyers/NDTr.svg?branch=master)](https://travis-ci.com/emeyers/NDTr)

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/emeyers/NDTr?branch=master&svg=true)](https://ci.appveyor.com/project/emeyers/NDTr)

[![Coveralls test
coverage](https://coveralls.io/repos/github/emeyers/NDTr/badge.svg)](https://coveralls.io/r/emeyers/NDTr?branch=master)

<!-- badges: end -->

<p>

## Overview

*Neural decoding* is a data analysis method that uses pattern
classifiers to predict experimental conditions based on neural activity.
The Neural Decoding Toolbox in R (NDTr) makes it easy to do neural
decoding analyses in R.

## Installation

You can install NDTr from github using:

``` r
# install.packages("devtools")
devtools::install_github("emeyers/NDTr")
```

## Usage

The package is based on 5 abstract object types:

1.  `Datasources (DS)`: generate training and test sets.
2.  `Feature preprocessors (FP)`: apply preprocessing to the training
    and test sets.
3.  `Classifiers (CL)`: learn relationships on the training set and make
    predictions on the test data.
4.  `Result Metrics (RM)`: summarize the prediction accuracies.
5.  `Cross-validators (CV)`: take the DS, FP and CL objects and run a
    cross-validation decoding procedure.

By combing different versions of these 5 object types together, it is
possible to run a range of different decoding analyses.

Below is a brief illustration of how to use the NDTr to do a simple
decoding analysis. To learn how to use the NDTr please see the
[introduction tutorial](introduction_tutorial.html), the [generalization
tutorial](generalization_tutorial.html) and the [documentation
website](https://emeyers.github.io/NDTr/).

``` r
library(NDTr)

# file to data in "binned format"
basedir_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package="NDTr")

# create the DS, FP, CL, RM, and CV objects
ds <- ds_basic(basedir_file_name, 'stimulus_ID', 5, num_label_repeats_per_cv_split = 3)
fps <- list(fp_zscore())
cl <- cl_max_correlation()
rms <- list(rm_main_results(aggregate_normalized_rank = "diag"), rm_confusion_matrix())
cv <- cv_standard(ds, cl, fps, rms, 3) 

# run a decoding analysis (this takes a few minutes) 
DECODING_RESULTS <- run_decoding(cv)
```

``` r
# plot the results for three different result types
plot(DECODING_RESULTS$rm_main_results, result_type = "all", plot_type = "line")
```

<img src="README-line_plot-1.png" style="display: block; margin: auto;" />

``` r
# create a temporal cross decoding plot
plot(DECODING_RESULTS$rm_main_results)
```

<img src="README-TCD_plot-1.png" style="display: block; margin: auto;" />

## Documentation

The documentation for this package is available at:
<https://emeyers.github.io/NDTr/>
