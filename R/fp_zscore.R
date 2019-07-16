#' A feature preprocessor (FP) that zscore normalizes the data
#'
#' This feature preprocessor object finds the mean and standard deviation using
#' the training data. The proprocessor then z-score transforms the training and
#' test data using this mean and standard deviation by subtracting the mean and
#' dividing by the standard deviation.
#'
#' @examples 
#' # The fp_zscore() constructor does not take any parameters. This
#' # object just needs to added to a list and passed to the cross-validator 
#' applied fp <- fp_zscore()
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
#'
#'
#' @family feature_preprocessor



# the constructor 
#' @export
fp_zscore <- function(){
  the_fp <- list()
  attr(the_fp, "class") <- "fp_zscore"
  the_fp
}



#' @export
preprocess_data.fp_zscore = function(fp, training_set, test_set){
  
  # separate the data from the labels
  train_labels <- select(training_set, -starts_with('site'))
  train_data <- select(training_set, starts_with('site'))
  
  test_labels <- select(test_set, -starts_with('site'))
  test_data <- select(test_set, starts_with('site'))
  

  # get the parameters and cale the rianing data (all in one line!)
  train_zscored <- scale(train_data, center = TRUE, scale = TRUE)
  
  # apply the parameters from trianing data to scale the test data
  test_centered <- sweep(test_data, 2, attr(train_zscored, "scaled:center") )
  test_zscored <- sweep(test_centered, 2, attr(train_zscored, "scaled:scale"), FUN = "/")
  
  
  # set to 0 all InF and NAs that could occur due to the standard deviation of a site being 0
  train_zscored[is.na(train_zscored)] <- 0
  test_zscored[is.na(test_zscored)] <- 0
  train_zscored[is.infinite(train_zscored)] <- 0     # perhaps should deal with the Infs differently...
  test_zscored[is.infinite(as.matrix(test_zscored))] <- 0
  
  # return a list with the results (after adding the labels back to the data)
  processed_data <- list(training_set = cbind(train_zscored, train_labels), 
                         test_set = cbind(test_zscored, test_labels))
  
  return(processed_data)
  
} 


# since there are no parameters for the fp_zscore just return a data frame with
# fp_zscore.fp_zscore and a value of "No parameters"
get_parameters.fp_zscore = function(fp_zscore){
  data.frame(fp_zscore.fp_zscore = "does not have settable parameters")
}



