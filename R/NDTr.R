#' NDTr: A package for neural decoding analyses
#'
#' The NDTr is a package that makes it easy to do neural decoding analyses.
#'
#' The NDTr is built around five abstract object types that work together in a
#' modular way to allow a range of neural decoding analyses. These five object
#' types are:
#'
#' 1. Datasources (DS): Generate training and test splits of the data.
#'
#' 2. Feature preprocessors (FP): Learn parameters on the training set and apply
#' transformations to the training and test sets.
#'
#' 3. Classifiers (CL): Learn the relationship between experimental conditions
#' (i.e., "labels") and neural data on a training set, and then predict
#' experimental conditions from neural data in a test set.
#'
#' 4. Result metrics (RM): Aggregate results across validation splits and
#' over resampled runs and compute and plot final decoding accuracy metrics.
#'
#' 5. Cross-validators (CV): Take the DS, FP, CL and RM objects and run a
#' cross-validation decoding procedure.
#'
#'
#' # Data formats
#'
#'  Two data formats are used to do decoding analyses which are:
#'
#'  1. `raster format` contains high temporal precision data where neural
#'  activity from each site is stored in a separate file.
#'
#'  2. `binned format` contains data from multiple sites where the data is more
#'  coarsely binned across time.
#'
#'  A user of the NDTr will typically store their data in `raster format` and
#'  then use the [create_binned_data()] to create a `binned format` data file
#'  that will be used in the decoding analysis.
#'
#'
#' @docType package
#' @name NDTr
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom stats cor pf predict


NULL


# making the where() function from the tidyselect package a global variable to
# get rid of a note in R CMD check because the authors of tidyselect have not
# exported this function yet (although according to a reply to a GitHub issue
# they are likely to do this soon)

globalVariables("where")
