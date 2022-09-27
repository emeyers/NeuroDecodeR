
# This file defines all the NDR generic functions. These functions are defined
# in this file because several S3 objects in the NDR use the same generic
# function, (e.g., the get_predictions() method is used by all classifier (CL)
# objects), so it makes the most sense to define them in a separate file rather
# than in the files that define particular NDR objects.



#' Get parameters of an NeuroDecodeR object
#'
#' Returns the parameters set in an NDR object to enable reproducible analyses.
#'
#' This function that returns a data frame with the parameters of an
#' NeuroDecodeR (NDR) object. All NDR objects (i.e., DS, FP, CL, RM and CV) need
#' to define a method that implements this generic function. The CV object's
#' `get_parameters()` method usually will call all the DS, FP, CL, RM and CV
#' `get_parameters()` methods and aggregate and return all the parameters
#' aggregated from these objects. These aggregated parameters can then be used
#' to save the results of a particular analysis based on the parameters using
#' the [log_save_results()] function. This method is most frequently used
#' privately by other NDR objects to save all the parameters that were used in
#' an analysis.
#'
#' @param ndr_obj An object from the NeuroDecodeR package to get the parameters from.
#'
#' @return Returns a data frame with a single row that contains all the NDR
#'   object's parameter values (e.g., values that were set in the object's
#'   constructor).
#'   
#' @keywords internal
#'
#' @export
get_parameters <- function(ndr_obj) {
  UseMethod("get_parameters")
}




#' A classifier (CL) method to train the CL and return predictions
#'
#' `get_predictions` takes a training set and a test set of data. It trains the
#' CL object on the training set and returns the predictions of the on the test
#' set. This is a generic function that must be implemented by all CL objects.
#' This method should not be called directly but instead it is used internally
#' by the cross-validator (CV) object.
#'
#' @param cl_obj The classifier object.
#'
#' @param training_set The training set data from one time bin. This is a data
#'   frame where the rows correspond to data from a given trial. There must be a
#'   column called `train_labels` that has the labels of what occurred on each
#'   trial. The rest of the  columns correspond to the neural activity of a
#'   particular site on each trial (and typically have names like site_0001,
#'   site_0002, etc).
#'
#' @param test_set The test set data from all times. This is a data frame where
#'   the rows correspond to data from a given trial. There must be a column
#'   called `time_bin` that contains a label indicating the time point that a
#'   row (test point) came from.  The rest of the  columns correspond to the
#'   neural activity of a particular site on each test trial (and typically have
#'   names like site_0001, site_0002, etc).
#'
#' @return This method returns a data frame where each row corresponds to a
#'   prediction for one of the test points. The columns in this data frame are:
#'   * _test_time_: The time bin a test point came from.
#'   * _actual_labels_: The actual labels for what happened on a trial.
#'   * _predicted_labels_: The predictions that classifier made.
#'   * \emph{decision_vals.___}: A set of columns with the decision values
#'   for each class.
#'
#' @keywords internal
#' @seealso [cl_max_correlation()], [cl_poisson_naive_bayes()], [cl_svm()]
#'
#' @export
get_predictions <- function(cl_obj, training_set, test_set) {
  UseMethod("get_predictions")
}





#' A datasource (DS) method to generate training and test sets
#'
#' This is a function that must be implemented by all DS objects. This method
#' should not be called directly but instead it is used internally by the
#' cross-validator (CV) object.
#'
#' @param ds_obj The datasource object.
#'
#' @return This method returns a data frame where each row corresponds to a
#'   data from one time point on a single trial.
#'
#'   *  \emph{train_labels}: The labels that should be used when training the classifier.
#'   *  \emph{test_labels}: The labels that should be used when the classifier
#'   is tested. Note, this can be different than the training labels when
#'   remapping the data using the [ds_generalization()] data source.
#'   * \emph{time_bin}: The time bin where the data point came from.
#'   * \emph{site_XXXX}: A set of columns with neural activity from each site.
#'   * \emph{CV_XX}: A set of columns that indicate for each
#'   cross-validation split whether a data point belongs to the training or test
#'   set.
#'
#' @keywords internal
#' @seealso [ds_basic()], [ds_generalization()]
#'
#' @export
get_data <- function(ds_obj) {
  UseMethod("get_data")
}





#' A feature-preprocessor (FP) method to pre-process training and test data
#'
#' This is a function that must be implemented by all FP objects. This object
#' learns a set of parameters from the training data (i.e., the data generated
#' from a datasource get_data() method). The `preprocess_data()` method then
#' uses these parameters do processing on the training and test data before the
#' data is sent to the classifier. This method should not be called directly but
#' instead it is used internally by the cross-validator (CV) object.
#'
#' @param fp The FP object.
#'
#' @param training_set The training set data from one time bin. This is a data
#'   frame where the rows correspond to data from a given trial. There must be a
#'   column called `train_labels` that has the labels of what occurred on each
#'   trial. The rest of the  columns correspond to the neural activity of a
#'   particular site on each trial (and typically have names like site_0001,
#'   site_0002, etc).
#'
#' @param test_set The test set data from all times. This is a data frame where
#'   the rows correspond to data from a given trial. There must be a column
#'   called `time_ bin` that contains a label indicating the time point that a
#'   row (test point) came from.  The rest of the  columns correspond to the
#'   neural activity of a particular site on each test trial (and typically have
#'   names like site_0001, site_0002, etc).
#'
#' @return A list is returned that contains two data frames called
#'   `training_set` and `test_set` which contain data in the same format as the
#'   `training_set` and `test_set` arguments passed to this function, however
#'   the data in these data frames has been processed by the FP object.
#'
#' @keywords internal
#' @seealso [fp_zscore()], [fp_select_k_features()]
#'
#' @export
preprocess_data <- function(fp, training_set, test_set) {
  UseMethod("preprocess_data")
}




#' A cross-validator (CV) method to run a decoding analysis
#'
#' This method runs a full decoding analysis based on the DS, FP, CL, and RM
#' objects that are passed to the cross-validator constructor.
#'
#' @param cv_obj A CV object. Parameters that affect the decoding analyses are
#'   set in the CV's constructor.
#'
#' @return A list, usually called `DECODING_RESULTS`, that contains the results
#'   from the decoding analysis. This `DECODING_RESULTS` list should contain the
#'   result compiled by the result metric objects, as well as a list in
#'   `DECODING_RESULTS$cross_validation_paramaters$parameter_df` contains data
#'   on all that DS, FP, CL and RM parameters that were used in the decoding
#'   analysis that can be used to store and retrieve the results. Additionally,
#'   the DS, FP, CL and RM objects used in the analysis can be saved in the
#'   `DECODING_RESULTS$cross_validation_paramaters`.
#'
#' @keywords internal
#' @seealso [cv_standard()]
#'
#' @export
run_decoding <- function(cv_obj) {
  UseMethod("run_decoding")
}





#' A result metric (RM) method to aggregate results over cross-validation splits
#'
#' This is a function that must be implemented by all RM objects. This function
#' is called by the cross-validator results aggregated across all
#' cross-validation splits. This method should not be called directly but
#' instead is used internally by the cross-validator (CV) object.
#' 
#' @param rm_obj The results metric object.
#'
#' @param prediction_results A data frame containing the prediction results to
#'   be aggregated over CV splits. The results in this data frame are the
#'   results returned by the CL's [get_predictions()] method, along with a
#'   column that specifies which cross-validation split the results come from.
#'   Thus the columns in the `prediction_results` data frame are: * _CV_: The
#'   cross-validation split number the results come from. * _test_time_: The
#'   time bin a test point comes from. * _actual_labels_: The actual labels for
#'   what happened on a trial. * _predicted_labels_: The predictions that
#'   classifier made. * \emph{decision_vals.___}: A set of columns with the
#'   decision values for each class returned by the classifier.
#'
#' @return A result-metric object that contains the decoding results aggregated
#'   across cross-validation splits, and thus should take up less memory than
#'   the original `prediction_results` that was passed in to this method.
#'
#' @keywords internal
#' @seealso [rm_main_results()], [rm_confusion_matrix()]
#'
#' @export
aggregate_CV_split_results <- function(rm_obj, prediction_results) {
  UseMethod("aggregate_CV_split_results")
}





#' A result metric (RM) method to aggregate results over resample runs
#'
#' This is a function that must be implemented by all RM objects. This function
#' is called by the cross-validator to aggregate results across all resample
#' runs. This method should not be called directly but instead it is used
#' internally by the cross-validator (CV) object.
#'
#' @param resample_run_results The decoding results from all resample runs.
#'
#' @return A result metric object that contains the decoding results aggregated
#'   across resample runs. This compressed final results can be plotted (often
#'   by using the RM plot methods).
#'
#' @keywords internal
#' @seealso [rm_main_results()], [rm_confusion_matrix()]
#'
#' @export
aggregate_resample_run_results <- function(resample_run_results) {
  UseMethod("aggregate_resample_run_results")
}
