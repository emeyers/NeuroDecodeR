


# the constructor 
#' @export
confusion_matrix_RM <- function(save_only_same_train_test_time = TRUE) {
  
  options <- list(save_only_same_train_test_time = save_only_same_train_test_time)
  
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
  
  
  # If specied in the constructur, save the confusion matrix only for training and testing 
  # the same times. This will save memory, and the off diagonal element confusion matrices 
  # can't generally of too much interest (however they could be of interest when 
  # converting the confusion matrix to mutual information). 
  
  options <- attr(confusion_matrix_obj, 'options')
  
  if (options$save_only_same_train_test_time) {
    prediction_results <- prediction_results %>% 
      dplyr::filter(train_time == test_time)  
  }
  
  
  # create the confusion matrix
  confusion_matrix <- prediction_results %>%
    dplyr::group_by(train_time, test_time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
  # # Adding this instead to the final aggregation step to save a little memory.
  # # However, doing it here has only a small advantage of making the confusion matrices 
  # # from all runs are the same size (but saving memory seems more important).
  # 
  # # add on 0's for all entries in the confusion matrix that are missing
  # empty_cm <-  expand.grid(resample_run = "0",
  #                          train_time = unique(confusion_matrix$train_time),
  #                          test_time = unique(confusion_matrix$test_time),
  #                          actual_labels = unique(confusion_matrix$actual_labels),
  #                          predicted_labels= unique(confusion_matrix$predicted_labels),
  #                          n = 0L, stringsAsFactors = FALSE)
  # 
  # confusion_matrix <- dplyr::bind_rows(confusion_matrix, empty_cm)
  # 
  # # add in the zero results...
  # confusion_matrix <-  confusion_matrix %>% 
  #   dplyr::group_by(train_time,  test_time, actual_labels,  predicted_labels) %>%
  #   summarize(n = sum(n))
  
  
  new_confusion_matrix_RM(confusion_matrix, 
                          'results combined over one cross-validation split', 
                          attr(confusion_matrix_obj, 'options'))
  
}




# aggregate the results from all the resample runs
#' @export
aggregate_resample_run_results.confusion_matrix_RM = function(resample_run_results) {


  confusion_matrix <- resample_run_results 

  
  # add on 0's for all entries in the confusion matrix that are missing

  options <- attr(resample_run_results, 'options')
  if (options$save_only_same_train_test_time) {

    # create smaller matrix of 0's if only saving results of training and testing at the same time
    cm_label_matrix <- expand.grid(actual_labels = unique(confusion_matrix$actual_labels),
                             predicted_labels = unique(confusion_matrix$predicted_labels))
    
    time_matrix <- data.frame(train_time = unique(confusion_matrix$train_time),
                              test_time = unique(confusion_matrix$test_time))
    
    empty_cm  <- data.frame(resample_run = "0",
                             time_matrix[rep(1:dim(time_matrix)[1], dim(cm_label_matrix)[1]), ],
                             cm_label_matrix[rep(1:dim(cm_label_matrix)[1], each = dim(time_matrix)[1]), ],
                             n = 0L)
    
    # could just filter the results below using train_time == test_time but this would require a lot more memory
    
  } else {
    
    empty_cm <-  expand.grid(resample_run = "0",
                             train_time = unique(confusion_matrix$train_time),
                             test_time = unique(confusion_matrix$test_time),
                             actual_labels = unique(confusion_matrix$actual_labels),
                             predicted_labels= unique(confusion_matrix$predicted_labels),
                             n = 0L, stringsAsFactors = FALSE)
  }

  
  
  confusion_matrix <- dplyr::bind_rows(confusion_matrix, empty_cm)

  confusion_matrix <-  confusion_matrix %>%
    dplyr::group_by(train_time,  test_time, actual_labels,  predicted_labels) %>%
    summarize(n = sum(n)) %>%
    dplyr::group_by(train_time,  test_time, actual_labels) %>%
    mutate(conditional_pred_freq = n / sum(n))    # Pr(predicted = y | actual = k)
  
  #dplyr::group_by(train_time,  test_time) %>%
  #mutate(predicted_frequency = n / sum(n))   # Pr(predicted = y, actual = k)
  
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

  # should perhaps give an option to choose a different color scale, and maybe other options? 
  
  # checking if only have the results for training and testing at the same time
  # could look at the 'options' attribute for this, but that won't help if the filtering happened at the
  # level of the cross-validator
  only_has_same_train_test_time_results <- 
    (sum(confusion_matrix_obj$train_time == confusion_matrix_obj$test_time) == dim(confusion_matrix_obj)[1])
  

  confusion_matrix_obj$train_time <- round(get_center_bin_time(confusion_matrix_obj$train_time))
  confusion_matrix_obj$test_time <- round(get_center_bin_time(confusion_matrix_obj$test_time))
  

  if (only_has_same_train_test_time_results) {
    
    # Add the word 'Time' to the title since there is enough space to plot it 
    # when only training and testing at the same time
    
    train_time_order <- paste('Time', unique(sort(confusion_matrix_obj$train_time)))
    confusion_matrix_obj$train_time <- ordered(
      paste('Time', confusion_matrix_obj$train_time),
      levels = train_time_order 
    )
    
    test_time_order <- paste('Time', unique(sort(confusion_matrix_obj$test_time)))
    confusion_matrix_obj$test_time <- ordered(
      paste('Time', confusion_matrix_obj$test_time),
      levels = test_time_order 
    )
    
  }
  

  g <- confusion_matrix_obj %>%
    ggplot(aes(predicted_labels, forcats::fct_rev(actual_labels), fill = conditional_pred_freq)) +
    geom_tile() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab('True class') + 
    xlab('Predicted class') +    # or should I transpose this (people do it differently...)
    scale_fill_continuous(type = "viridis", name = "Prediction\n accuracy") #+ 

  if (sum(confusion_matrix_obj$train_time == confusion_matrix_obj$test_time) == dim(confusion_matrix_obj)[1]){
        g + facet_wrap(~train_time)
  } else {    
      g + facet_grid(train_time ~ test_time)
  } 
  
  
  
}














