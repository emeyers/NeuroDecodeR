



# the constructor 
#' @export
confusion_matrix_RM <- function(options = NULL) {
  
  new_confusion_matrix_RM(data.frame(), 'initial', options)

}





# aggregate the results from all the cross-validation splits
#' @export
aggregate_CV_split_results.confusion_matrix_RM = function(confusion_matrix_obj, prediction_results) {
  

  # include a warning if the state is not intial
  if (attr(confusion_matrix_obj, "state") != "initial") {    
    warning(paste0("The method aggregate_CV_split_results() should only be called on",
                   "normalized_rank_and_decision_values_RM that are in the intial state.",
                   "Any data that was already stored in this object will be overwritten"))
  }
  
  
  # could only get confusion matrix where train and test times are the same to save memory
  # (off diagonal element confusion matrices don't seem that much of interest anyway)
  # however MI off diagonal could be of interest so going to get all trian test times confusion matrices
  
  confusion_matrix <- prediction_results %>%
    #dplyr::filter(train_time == test_time)  %>%  
    dplyr::group_by(train_time, test_time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
  # Could do this in the final aggregation step which might save a little memory.
  # However, doing it here to make confusion matrices from all runs are the same size 
  #  which could have advantages.
  
  # add on 0's for all entries in the confusion matrix that are missing
  empty_cm <-  expand.grid(resample_run = "0",
                           train_time = unique(confusion_matrix$train_time),
                           test_time = unique(confusion_matrix$test_time),
                           actual_labels = unique(confusion_matrix$actual_labels),
                           predicted_labels= unique(confusion_matrix$predicted_labels),
                           n = 0L, stringsAsFactors = FALSE)
  
  confusion_matrix <- dplyr::bind_rows(confusion_matrix, empty_cm)
  
  # add in the zero results...
  confusion_matrix <-  confusion_matrix %>% 
    dplyr::group_by(train_time,  test_time, actual_labels,  predicted_labels) %>%
    summarize(n = sum(n))
  
  
  new_confusion_matrix_RM(confusion_matrix, 
                          'results combined over one cross-validation split', 
                          attr(confusion_matrix_obj, 'options'))
  
}




# aggregate the results from all the resample runs
#' @export
aggregate_resample_run_results.confusion_matrix_RM = function(resample_run_results) {


  confusion_matrix <- resample_run_results # dplyr::bind_rows(resample_run_results, .id = "resample_run")

  # # add on 0's for all entries in the confusion matrix that are missing
  # empty_cm <-  expand.grid(resample_run = "0",
  #                          train_time = unique(confusion_matrix$train_time),
  #                          test_time = unique(confusion_matrix$test_time),
  #                          actual_labels = unique(confusion_matrix$actual_labels),
  #                          predicted_labels= unique(confusion_matrix$predicted_labels),
  #                          n = 0L, stringsAsFactors = FALSE)
  #
  # confusion_matrix <- dplyr::bind_rows(confusion_matrix, empty_cm)


  confusion_matrix <-  confusion_matrix %>%
    dplyr::group_by(train_time,  test_time, actual_labels,  predicted_labels) %>%
    summarize(n = sum(n)) %>%
    dplyr::group_by(train_time,  test_time) %>%
    mutate(predicted_frequency = n / sum(n)) 
  
  new_confusion_matrix_RM(confusion_matrix, 
                          'final results', 
                          attr(resample_run_results, 'options'))
  
}




# the internal constructor
new_confusion_matrix_RM <- function(the_data = data.frame(), 
                                    the_state = NULL,
                                    options = NULL) {
  
    confusion_matrix_obj <- the_data
    attr(confusion_matrix_obj, "state") <- the_state
    attr(confusion_matrix_obj, "options") <- options
    attr(confusion_matrix_obj, "class") <- c("confusion_matrix_RM", 'data.frame')
    
    confusion_matrix_obj
  
}




# aggregate the results from all the resample runs
#' @export
plot.confusion_matrix_RM = function(confusion_matrix_obj) {
  
  confusion_matrix_obj %>%
    ggplot(aes(actual_labels, predicted_labels, fill = n)) +
    geom_tile() +
    facet_grid(train_time ~ test_time)
}






get_mutual_information = function(prediction_results) {
  
  
  browser()
  
  
  blah <- prediction_results %>%
    dplyr::group_by(time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
}


















