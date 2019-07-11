#' A feature preprocessor (FP) that zscore normalizes the data
#'
#' This feature preprocessor object find the mean and standard deviation using the training data. 
#' The proprocessor then z-score transforms the training and test data using this mean and standard deviation
#' by subtracting the mean and dividing by the standard deviation. 
#'
#'
#' @export



# the constructor 
#' @export
zscore_FP <- function(){
  the_fp <- list()
  attr(the_fp, "class") <- "zscore_FP"
  the_fp
}



#' @export
preprocess_data.zscore_FP = function(fp, training_set, test_set){
  
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


# since there are no parameters for the zscore_FP just return a data frame with
# zscore_FP.zscore_FP and a value of "No parameters"
get_parameters.zscore_FP = function(zscore_FP){
  data.frame(zscore_FP.zscore_FP = "does not have settable parameters")
}



