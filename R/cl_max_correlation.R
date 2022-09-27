#' A maximum correlation coefficient classifier (CL)
#'
#' An implementation of a maximum correlation coefficient classifier.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the cl_maximum_correlation classifier work with the pipe
#'   (|>) operator. This argument should almost never be directly set by the
#'   user to anything other than NULL. If this is set to the default value of
#'   NULL, then the constructor will return a cl_max_correlation object. If this
#'   is set to an NDT container, then a cl_max_correlation object will be added
#'   to the container and the container will be returned. If this argument is
#'   set to another NDT object, then both that NDR object as well as a new
#'   cl_maximum_correlation object will be added to a new container and the
#'   container will be returned.
#'   
#' @param return_decision_values A Boolean specifying whether the prediction
#'   function should return columns that have the decision values. Setting this
#'   to FALSE will save memory so can be useful when analyzing very large high
#'   temporal resolution data sets. However if this is set to FALSE< metrics
#'   won't be able to compute decoding accuracy measures that are based on the
#'   decision values; e.g., the rm_main_results object won't be able to
#'   calculate normalized rank decision values.
#'
#' @return This constructor creates an NDR classifier object with the class
#'   `cl_max_correlation`. Like all NDR classifier objects, this classifier will
#'   be used by a cross-validator to learn the relationship between neural
#'   activity and experimental conditions on a training set of data, and then it
#'   will be used to make predictions on a test set of data.
#'
#' @details This CL object learns a mean population vector (template) for each
#'   class from the training set (by averaging together the all training points
#'   within each class). The classifier is tested by calculated Pearson’s
#'   correlation coefficient between a test point and the templates learned from
#'   the training set, and the class with the highest correlation value is
#'   returned as the predicted label. The decision values returned by the
#'   classifier are the correlation coefficients between all test points and all
#'   templates.
#'
#' Like all classifiers (CL) objects, this classifier has a private
#' get_predictions() method which learns a model based on training data and then
#' makes predictions on the test data.
#'
#'
#' @examples
#' # running a basic decoding analysis using the cl_max_correlation
#' data_file <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"),
#'                          package = "NeuroDecodeR")
#' ds <- ds_basic(data_file, "stimulus_ID", 18)
#' fps <- list(fp_zscore())
#'
#' cl <- cl_max_correlation()
#' cv <- cv_standard(datasource = ds, 
#'                   classifier = cl, 
#'                   feature_preprocessors = fps,
#'                   num_resample_runs = 2)  # better to use more resample runs (default is 50)
#' \donttest{
#' DECODING_RESULTS <- run_decoding(cv)
#' }
#'
#' @family classifier


# the constructor
#' @export
cl_max_correlation <- function(ndr_container_or_object = NULL,
                               return_decision_values = TRUE) {
  
  if (!is.logical(return_decision_values)) {
    stop("return_decision_values must be set to either TRUE or FALSE")
  }
  
  
  the_classifier <- list(return_decision_values = return_decision_values)
  attr(the_classifier, "class") <- "cl_max_correlation"  # c("cl_max_correlation", "ndr_object")

  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the classifier in it
  put_ndr_object_in_container(ndr_container_or_object, the_classifier)
    
}  # end the cl_max_correlation constructor






# the get_predictions method
#' @inherit get_predictions
#' @keywords internal
#' @export
get_predictions.cl_max_correlation <- function(cl_obj,
                                               training_set,
                                               test_set) {

  ### Train the classifier  ---------------------------------------------------
  prototypes <- training_set %>%
    dplyr::group_by(.data$train_labels) %>%
    dplyr::summarise_all(mean)



  ### Test the classifier  ---------------------------------------------------
  train_test_cor <- cor(
    t(prototypes[, 2:dim(prototypes)[2]]),
    t(dplyr::select(test_set, -.data$test_labels, -.data$time_bin))
  )

  # get the predicted labels
  predicted_inds <- apply(train_test_cor, 2, rand_which_max)
  predicted_labels <- prototypes$train_labels[predicted_inds]


  if (sum(is.na(predicted_labels))) {
    warning("some of the predicted results returned by the max correlation classifier are NAs")
  }


  # create a data frame that has all the results
  results <- data.frame(
    test_time = test_set$time_bin,
    actual_labels = test_set$test_labels,
    predicted_labels = predicted_labels)

  
  # return the decision values along with the zero-one loss results
  if (cl_obj$return_decision_values) {
    
    decision_values <- data.frame(t(train_test_cor))
    names(decision_values) <- paste0("decision_vals.", prototypes$train_labels)
    
    results <- cbind(results, decision_values)
  }


  return(results)
  
}





# since there are no parameters for the cl_max_correlation, just return a data
# frame with one variable with a value that there are not settable parameters
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.cl_max_correlation <- function(ndr_obj) {
  data.frame(cl_max_correlation.cl_max_correlation = "does not have settable parameters")
}
