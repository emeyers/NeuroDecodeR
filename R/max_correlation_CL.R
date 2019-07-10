#' A maximum correlation coefficient classifier (CL) object
#'
#' An implementation of a maximum correlation coefficeint classifier. 
#' Like all classifiers (CL) objects, this classifier has a get_predictions()
#' method which learns a model based on training data and then makes 
#' predictions on the test data.  
#'
#'
#' 
#' @export



# the constructor 
#' @export
max_correlation_CL <- function(){
  the_classifier <- list()
  attr(the_classifier, "class") <- "max_correlation_CL"
  the_classifier
}


# the get_predictions method
#' @export
get_predictions.max_correlation_CL <- function(max_correlation_CL_obj, 
                                               train_data, 
                                               all_times_test_data) {  

    
  ### Train the classifier  ---------------------------------------------------
  prototypes <- train_data %>% 
    dplyr::group_by(labels) %>% 
    dplyr::summarise_all(mean)
  

  
  ### Test the classifier  ---------------------------------------------------
  train_test_cor <- cor(t(prototypes[, 2:dim(prototypes)[2]]), 
                        t(dplyr::select(all_times_test_data, -labels, -time_bin)))
  
  # get the predicted labels
  predicted_inds <- apply(train_test_cor, 2, rand_which_max)
  predicted_labels <- prototypes$labels[predicted_inds]
  
  # create a data frame that has all the results
  results <- data.frame(test_time = all_times_test_data$time_bin, 
                        actual_labels = all_times_test_data$labels, 
                        predicted_labels = predicted_labels) # %>%
    # dplyr::mutate(correct = actual_labels == predicted_labels)
  
  # get the decision values
  decision_values <- data.frame(t(train_test_cor))
  names(decision_values) <- paste0('decision_vals.', prototypes$labels)  
  
  # return the results
  results <- cbind(results, decision_values)
  
  return(results)
  
} 


# since there are no parameters for the max_correlation_CL just return a data frame with
# max_correlation_CL.max_correlation_CL and a value of "no parameters"
get_parameters.max_correlation_CL = function(max_correlation_CL_obj){
  data.frame(max_correlation_CL.max_correlation_CL = "no parameters")
}












