#' A feature preprocessor (FP) that z-score normalizes the data
#'
#' This feature preprocessor object finds the mean and standard deviation using
#' the training data. The preprocessor then z-score transforms the training and
#' test data using this mean and standard deviation by subtracting the mean and
#' dividing by the standard deviation.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the fp_zscore feature preprocessor work with the 
#'   pipe (|>) operator. This argument should almost never be directly set by
#'   the user to anything other than NULL. If this is set to the default value
#'   of NULL, then the constructor will return a fp_zscore object. If this is
#'   set to an ndr container, then a fp_zscore object will be added to the
#'   container and the container will be returned. If this argument is set to
#'   another ndr object, then both that ndr object as well as a new fp_zscore
#'   object will be added to a new container and the container will be returned.
#'   
#' @return This constructor creates an NDR feature preprocessor object with the
#'   class `fp_zscore`. Like all NDR feature preprocessor objects, this feature
#'   preprocessor will be used by the cross-validator to pre-process the
#'   training and test data sets.
#'   
#'
#' @details This feature preprocessor object applies z-score normalization to
#' each feature by calculating the mean and the standard deviation for each
#' feature using the training data, and then subtracting the mean and dividing
#' by the standard deviation for each feature in the training and test sets.
#' This function is useful for preventing some classifiers from relying too
#' heavily on particular features when different features can have very
#' different ranges of values (for example, it is useful when decoding neural
#' data because different neurons can have different ranges of firing rates).
#'
#' @examples
#' # The fp_zscore() constructor does not take any parameters. This object
#' # just needs to added to a list and passed to the cross-validator applied
#' fp <- fp_zscore()
#' @family feature_preprocessor
#'
#'
#'
#' @export
fp_zscore <- function(ndr_container_or_object = NULL) {
  
  the_fp <- list()
  attr(the_fp, "class") <- "fp_zscore"
  
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the feature preprocessor in it
  put_ndr_object_in_container(ndr_container_or_object, the_fp)
  
}



#' @inherit preprocess_data
#' @keywords internal
#' @export
preprocess_data.fp_zscore <- function(fp, training_set, test_set) {

  # separate the data from the labels
  train_labels <- select(training_set, -starts_with("site"))
  train_data <- select(training_set, starts_with("site"))

  test_labels <- select(test_set, -starts_with("site"))
  test_data <- select(test_set, starts_with("site"))


  # get the parameters and cale the rianing data (all in one line!)
  train_zscored <- scale(train_data, center = TRUE, scale = TRUE)


  # apply the parameters from trianing data to scale the test data
  #  (much faster way to z-score the data compared to using the sweep function)
  test_zscored <- scale(test_data,
    center = attr(train_zscored, "scaled:center"),
    scale = attr(train_zscored, "scaled:scale")
  )


  # set to 0 all InF and NAs that occur due to the sd of a site being 0
  train_zscored[is.na(train_zscored)] <- 0
  test_zscored[is.na(test_zscored)] <- 0
  train_zscored[is.infinite(train_zscored)] <- 0 # could deal with the Infs differently...
  test_zscored[is.infinite(as.matrix(test_zscored))] <- 0

  # return a list with the results (after adding the labels back to the data)
  processed_data <- list(
    training_set = cbind(train_zscored, train_labels),
    test_set = cbind(test_zscored, test_labels)
  )

  return(processed_data)
}




# since there are no parameters for the fp_zscore just return a data frame with
# fp_zscore.fp_zscore and a value of "No parameters"
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.fp_zscore <- function(ndr_obj) {
  data.frame(fp_zscore.fp_zscore = "does not have settable parameters")
}
