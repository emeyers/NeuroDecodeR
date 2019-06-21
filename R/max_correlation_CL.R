#' A maximum correlation coefficient classifier (CL) object
#'
#' An implementation of a maximum correlation coefficeint classifier. Like all classifiers, this classifier
#' learning a model based on training data and then makes predictions on new test data.  
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
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
get_predictions.max_correlation_CL <- function(max_correlation_CL_obj, train_data, all_times_test_data) {  
  
  
  ### Train the classifier  ---------------------------------------------------
  prototypes <- train_data %>% group_by(labels) %>% summarise_all(funs(mean))
  
  
  ### Test the classifier  ---------------------------------------------------
  train_test_cor <- cor(t(prototypes[, 2:dim(prototypes)[2]]), 
                        t(dplyr::select(all_times_test_data, -labels, -time)))

  # get the predicted labels
  predicted_inds <- apply(train_test_cor, 2, rand_which_max)
  predicted_labels <- prototypes$labels[predicted_inds]
  
  # create a data frame that has all the results
  results <- data.frame(time = all_times_test_data$time, 
                        actual_labels = all_times_test_data$labels, 
                        predicted_labels = predicted_labels) %>%
    dplyr::mutate(correct = actual_labels == predicted_labels)
  
  # get the decision values
  decision_values <- data.frame(t(train_test_cor))
  names(decision_values) <- paste0('decision_val_', prototypes$labels)  
  
  # return the results
  results <- cbind(results, decision_values)
  
  return(results)
  
} 















