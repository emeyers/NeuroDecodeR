#' A maximum correlation coefficient classifier (CL) object
#'
#' An implementation of a maximum correlation coefficeint classifier. Like all
#' classifiers (CL) objects, this classifier has a get_predictions() method which
#' learns a model based on training data and then makes predictions on the test
#' data.
#'
#' @details This CL object learns a mean population vector (template) for each
#' class from the training set (by averaging together the all training points
#' within each class). The classifier is tested by calculated Pearson’s
#' correlation coefficient between a test point and the templates learned from
#' the training set, and the class with the highest correlation value is returned
#' as the predicted label. The decision values returned by the classifier are the
#' correlation coefficients between all test points and all templates.
#'
#'
#' @examples # running a basic decoding analysis using the cl_max_correlation
#' ds <- ds_basic(basedir_file_name, 'stimulus_ID', 18) fps <- list(fp_zscore())
#' cl <- cl_max_correlation() rms <- rms <- list(rm_main_results(),
#' rm_confusion_matrix()) cv <- cv_standard(ds, cl, fps, 3, rms)
#'
#'
#' @family classifier




# the constructor 
#' @export
cl_max_correlation <- function(){
  the_classifier <- list()
  attr(the_classifier, "class") <- "cl_max_correlation"
  the_classifier
}


# the get_predictions method
#' @export
get_predictions.cl_max_correlation <- function(cl_max_correlation_obj, 
                                               train_data, 
                                               all_times_test_data) {  

    
  ### Train the classifier  ---------------------------------------------------
  prototypes <- train_data %>% 
    dplyr::group_by(train_labels) %>% 
    dplyr::summarise_all(mean)
  

  
  ### Test the classifier  ---------------------------------------------------
  train_test_cor <- cor(t(prototypes[, 2:dim(prototypes)[2]]), 
                        t(dplyr::select(all_times_test_data, -test_labels, -time_bin)))
  
  # get the predicted labels
  predicted_inds <- apply(train_test_cor, 2, rand_which_max)
  predicted_labels <- prototypes$train_labels[predicted_inds]
  
  # create a data frame that has all the results
  results <- data.frame(test_time = all_times_test_data$time_bin, 
                        actual_labels = all_times_test_data$test_labels, 
                        predicted_labels = predicted_labels) # %>%
    # dplyr::mutate(correct = actual_labels == predicted_labels)
  
  # get the decision values
  decision_values <- data.frame(t(train_test_cor))
  names(decision_values) <- paste0('decision_vals.', prototypes$train_labels)  
  
  # return the results
  results <- cbind(results, decision_values)
  
  return(results)
  
} 


# since there are no parameters for the cl_max_correlation just return a data frame with
# cl_max_correlation.cl_max_correlation and a value of "no parameters"
get_parameters.cl_max_correlation = function(cl_max_correlation_obj){
  data.frame(cl_max_correlation.cl_max_correlation = "does not have settable parameters")
}












