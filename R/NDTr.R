#' NDTr: A package for doing neural decoding
#'
#' The NDTr is a package that makes it easy to do neural decoding analyses
#' 
#' The NDTr is built around 5 abstract object types that work together in a
#' modular way to allow a range of neural decoding analyses. These five object
#' types are:
#' 
#' @section 1. Datasources (DS): 
#' Generate training and test splits of the data
#'
#' @section 2. Feature preprofessors (FP):
#'  Learn parameters on the training set and
#'   apply transformations to the training and test sets
#'
#' @section 3. Classifiers (CL): 
#' learn the relationship between experimental
#'   conditions and data on the training set, and then predict experimental
#'   conditions on the test data.
#'   
#' @section 4. Result metrics (RM): 
#'  Aggregate results from across validation splits and over resampled runs and
#'  compute and plot final decoding accuracy metrics
#'
#' @section 5. Cross-validators (CV):
#'  Take the DS, FP and CL objects and run a cross-validation decoding
#'  procedure.
#'  
#' @section Data formats: 
#'  Two data formats are used: 1) 'raster format' which contains high temporal
#'  precision data where each site is stored in a spearate file and 2) 'binned
#'  format' where data more coarsely binned across time. The function
#'  created_binned_data() converts data from raster format to binned format.
#'
#' @docType package
#' @name NDTr
#'
#' @import parallel  
#' @import doParallel
#' @import foreach
#' @import dplyr
#' @import e1071 
#' @import ggplot2
#' @import magrittr
#' @importFrom stats cor pf predict




NULL






