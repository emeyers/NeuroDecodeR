

# Becuase the generic functions are used by several diffent S3 objects, 
#  (e.g., the get_predictions() method is used by all classifier (CL) objects),
#  I am registering all generic functions in this file here rather than in the files
#  that define particular objects. 



#' Return all parameters that an object uses to enable reproducible analyses
#' @export
#' @param obj The object that one should get the parameters from
get_parameters <- function(obj) {
  UseMethod("get_parameters")
}



#' Register the generic function get_predictions for the classifier (CL) objects
#' @param cl_obj The classifier object
#' @param training_set The training data
#' @param test_set The test data from all times
#' @export
get_predictions <- function(cl_obj, training_set, test_set) {
  UseMethod("get_predictions")
}



#' Register the generic function get_data() for datasource (DS) objects
#' Need to fill in more complete documentation here about the DS interface
#' @param ds_obj The data source object
#' @export
get_data <- function(ds_obj) {
  UseMethod("get_data")
}



#' Register the generic function preprocess_data() for feature preprocessor (FP) objects
#' Need to fill in more complete documentation here about the FP interface
#' @param fp The FP object
#' @param training_set The training set for a particular time
#' @param test_set The test set from all times
#' @export
preprocess_data <- function(fp, training_set, test_set) {
  UseMethod("preprocess_data")
}




#' Register the generic function run_decoding
#' Need to fill in more complete documentation here about the CV interface
#' @param cv_obj The CV object
#' @export
run_decoding <- function(cv_obj) {
  UseMethod("run_decoding")
}




#' The required aggregate_resample_run_results method is needed to fulfill the
#' metric object interface. This function is called by the cross-validator to 
#' aggregrate results across all cross-validation splits. Note: this function should not be
#' called by users of the package but instead is called interally by cross-validator
#' objects.
#' @param rm_obj The results metric object
#' @param prediction_results the prediction results to be aggregated over CV splits
#' @export
aggregate_CV_split_results <- function(rm_obj, prediction_results) {
  UseMethod("aggregate_CV_split_results")
}





#' The required aggregate_resample_run_results method is needed to fulfill the
#' metric object interface. This function is called by the cross-validator to 
#' aggregrate results across all resample runs. Note: this function should not be
#' called by users of the package but instead is called interally by cross-validator
#' objects.
#' @param resample_run_results The decoding results from all resample runs
#' @export
aggregate_resample_run_results <- function(resample_run_results) {
    UseMethod("aggregate_resample_run_results")
}



#' Plot the mutual information computed from a confusion matrix
#' @param obj the RM object
#' @param plot_type The type of plot that should be displayed
#' @export
plot_MI <- function(obj, plot_type) {
  UseMethod("plot_MI")
}

  




