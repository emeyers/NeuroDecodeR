#' A maximum correlation coefficient classifier (CL)
#'
#' An implementation of a maximum correlation coefficient classifier.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the cl_maximum_corrleation classifier work with the
#'   magrittr pipe (%>%) operator. This argument should almost never be directly
#'   set by the user to anything other than NULL. If this is set to the default
#'   value of NULL, then the constructor will return a cl_max_correlation
#'   object. If this is set to an ndr container, then a cl_max_correlation
#'   object will be added to the container and the container will be returned.
#'   If this argument is set to another ndr object, then both that ndr object as
#'   well as a new cl_maximum_corrleation object will be added to a new
#'   container and the container will be returned.
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
#' Like all classifiers (CL) objects, this classifier has a get_predictions()
#' method which learns a model based on training data and then makes predictions
#' on the test data.
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
#'                   feature_preprocessors = fps)
#' \dontrun{
#' DECODING_RESULTS <- run_decoding(cv)
#' }
#'
#' @family classifier


# the constructor
#' @export
cl_max_correlation <- function(ndr_container_or_object = NULL) {
  
  the_classifier <- list()
  attr(the_classifier, "class") <- "cl_max_correlation"  # c("cl_max_correlation", "ndr_object")

  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the classifier in it
  put_ndr_object_in_container(ndr_container_or_object, the_classifier)
    
}  # end the cl_max_correlation constructor






# the get_predictions method
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

  # get the decision values
  decision_values <- data.frame(t(train_test_cor))
  names(decision_values) <- paste0("decision_vals.", prototypes$train_labels)

  # return the results
  results <- cbind(results, decision_values)

  return(results)
}


# since there are no parameters for the cl_max_correlation, just return a data
# frame with one variable with a value that there are not settable parameters
#' @export
get_parameters.cl_max_correlation <- function(ndr_obj) {
  data.frame(cl_max_correlation.cl_max_correlation = "does not have settable parameters")
}
