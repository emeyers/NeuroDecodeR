

# Becuase the generic functions are used by several diffent S3 objects, 
#  (e.g., the get_predictions() method is used by all classifier (CL) objects),
#  I am registering all generic functions in this file here rather than in the files
#  that define particular objects. 



# register the generic function get_predictions for the classifier (CL) objects
#' @export
get_predictions <- function(obj, train_data, all_times_test_data) {
  UseMethod("get_predictions")
}


# register the generic function get_data() for datasource (DS) objects
#' @export
get_data <- function(ds_obj) {
  UseMethod("get_data")
}


# register the generic function preprocess_data() for feature preprocessor (FP) objects
#' @export
preprocess_data <- function(fp_obj, train_data, all_times_test_data) {
  UseMethod("preprocess_data")
}


# register the generic function preprocess_data() for feature preprocessor (FP) objects
#' @export
run_decoding <- function(cv_obj) {
  UseMethod("run_decoding")
}












