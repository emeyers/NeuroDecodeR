



# the constructor 
#' @export
confusion_matrix_PM <- function(the_data = data.frame(), state = 'initial'){
  
  confusion_matrix_obj <- the_data
  
  attr(confusion_matrix_obj, "class") <- c("confusion_matrix_PM", 'data.frame')
  
  attr(confusion_matrix_obj, "state") <- state
  
  confusion_matrix_obj
  
}





# aggregate the results from all the cross-validation splits
#' @export
aggregate_CV_split_results.confusion_matrix_PM = function(confusion_matrix_obj, prediction_results) {
  

  # could only get confusion matrix where train and test times are the same to save memory
  # (off diagonal element confusion matrices don't seem that much of interest anyway)
  # however MI off diagonal could be of interest so going to get all trian test times confusion matrices
  
  confusion_matrix <- prediction_results %>%
    #dplyr::filter(train_time == test_time)  %>%  
    dplyr::group_by(train_time, test_time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
  confusion_matrix_PM(confusion_matrix_PM, 'aggregated_CV_data')
  
  
  # cool, can plot the confusion matrix...
  # confusion_matrix %>%
  #   ggplot(aes(actual_labels, predicted_labels, fill = n)) +
  #   geom_tile() +
  #   facet_grid(train_time ~ test_time)
  
  
}





get_mutual_information = function(prediction_results) {
  
  
  browser()
  
  
  blah <- prediction_results %>%
    dplyr::group_by(time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
}


















