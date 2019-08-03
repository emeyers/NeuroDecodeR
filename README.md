
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/NDTr.svg?branch=master)](https://travis-ci.com/muschellij2/NDTr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/NDTr?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/NDTr)
[![Coveralls test
coverage](https://coveralls.io/repos/github/emeyers/NDTr/badge.svg)](https://coveralls.io/r/emeyers/NDTr?branch=master)
<!-- badges: end -->

<p>

## Description

The Neural Decoding Toolbox in R (NDTr) package allows one to do a
neural decoding analysis in R.

The package is based on 5 abstract object types:

1.  datasources (DS)
2.  features preprocessors (FP)
3.  classifiers (CL)
4.  result metrics (RM)
5.  cross-validators (CV)

By combing different versions of these 4 objecttypes together, it is
possible to run a range of different decoding analyses. See
www.readout.info and <https://emeyers.github.io/NDTr/> for more
information.

## Installation

You can install NDTr from github using:

``` r
# install.packages("devtools")
devtools::install_github("emeyers/NDTr")
```

## Documentation

The documetnation for this package is available at:
<https://emeyers.github.io/NDTr/>
