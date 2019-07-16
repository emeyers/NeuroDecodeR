



# the constructor 
#' @export
rm_main_results <- function(the_data = data.frame(), state = 'initial', options = NULL){
  
  main_results_obj <- the_data
  
  attr(main_results_obj, "class") <- c("rm_main_results", 'data.frame')
  
  attr(main_results_obj, "state") <- state

  attr(main_results_obj, "options") <- options
  
  main_results_obj
  
}





# aggregate the results from all the cross-validation splits
#' @export
aggregate_CV_split_results.rm_main_results = function(main_results_obj, prediction_results) {
  
  
  # perhaps include a warning if the state is not intial
  if (attr(main_results_obj, "state") != "initial") {    
    warning(paste0("The method aggregate_CV_split_results() should only be called on",
                   "rm_main_results that are in the intial state.",
                   "Any data that was already stored in this object will be overwritten"))
  }

  
  # calculate which predictions were correct for the zero-one loss metric
  prediction_results <- prediction_results %>%
     dplyr::mutate(correct = actual_labels == predicted_labels)
  
  
  decision_vals <- select(prediction_results, starts_with("decision"))
  num_classes <- ncol(decision_vals)
  num_test_points <- nrow(decision_vals)
  
  
  # remove the prefix 'decision_vals' from the column names...
  the_names <- names(decision_vals)
  the_names <- unlist(strsplit(the_names, "decision_vals.", fixed = TRUE))
  the_names <- the_names[the_names != ""]
  names(decision_vals) <- the_names
  decision_vals_aug <- cbind(prediction_results$actual_labels, decision_vals)
  
  
  get_rank_one_row <- function(decision_vals_aug_row) {
    actual_label <- decision_vals_aug_row[1]
    decision_vals_row <- decision_vals_aug_row[2:length(decision_vals_aug_row)]
    the_names <- names(decision_vals_row)
    the_order <- order(as.numeric(decision_vals_row), decreasing = TRUE)
    which(the_names[the_order] == actual_label)
  }
  
  normalized_rank_results <- 1 - ((apply(decision_vals_aug, 1, get_rank_one_row) - 1)/(num_classes - 1))
  
  
  # get the decision values for the correct label
  get_decision_vals_one_row <- function(decision_vals_aug_row) {
    decision_vals_aug_row[which(as.character(as.matrix(decision_vals_aug_row[1])) == names(decision_vals_aug_row[2:length(decision_vals_aug_row)])) + 1]
  }
  
  correct_class_decision_val <- as.numeric(apply(decision_vals_aug, 1, get_decision_vals_one_row))
  
  

  the_results <- cbind(dplyr::select(prediction_results, -starts_with("decision_val")),
                       data.frame(decision_values = correct_class_decision_val,
                            normalized_rank_results = normalized_rank_results))

  the_results <- the_results %>%                 
    group_by(CV, train_time, test_time) %>%
    summarize(zero_one_loss = mean(correct),
              normalized_rank = mean(normalized_rank_results),
              decision_vals = mean(decision_values))

  
  rm_main_results(the_results, 'results combined over one cross-validation split', attributes(main_results_obj)$options)
  
}




# aggregate the results from all the resample runs
#' @export
aggregate_resample_run_results.rm_main_results = function(resample_run_results) {
  
  
  central_results <- resample_run_results %>%                 
    group_by(train_time, test_time) %>%
    summarize(zero_one_loss = mean(zero_one_loss),
              normalized_rank = mean(normalized_rank),
              decision_vals = mean(decision_vals))
  
  
  rm_main_results(central_results, 'final results', attributes(resample_run_results)$options)
  
  
}



# plot results (TCT plot for now)
#' @export
plot.rm_main_results = function(main_results, result_type = 'all', plot_type = 'TCD') {
  
  
  if (result_type == 'all'){ 
    # do nothing
  } else if (result_type == 'zero_one_loss'){
    main_results <- select(main_results, train_time, test_time, zero_one_loss)
  } else if (result_type == 'normalized_rank'){
    main_results <- select(main_results, train_time, test_time, normalized_rank)
  } else if (result_type == 'decision_vals'){
    main_results <- select(main_results, train_time, test_time, decision_vals)
  } else {
    warning(paste0("result_type must be set to either 'all', 'zero_one_loss', 'normalized_rank', or 'decision_vals'.",
                   "Using the default value of all"))
  }
  
  
  if (!(plot_type == 'TCD' || plot_type == 'line'))
      warning("plot_type must be set to 'TCD' or 'line'. Using the default value of 'TCD'")
  
  
  main_results$train_time <- round(get_center_bin_time(main_results$train_time))
  main_results$test_time <- round(get_center_bin_time(main_results$test_time))
  
  #main_results$train_time <- get_time_range_strings(main_results$train_time)
  #main_results$test_time <- get_time_range_strings(main_results$test_time)
  
  
  main_results <-  main_results %>%
    tidyr::gather(result_type, accuracy, -train_time, -test_time) %>%
    dplyr::mutate(result_type = replace(result_type, result_type == 'zero_one_loss', 'Zero-one loss'),
                 result_type = replace(result_type, result_type == 'normalized_rank', 'Normalized rank'),
                 result_type = replace(result_type, result_type == 'decision_vals', 'Decision values'))
    

  if ((sum(main_results$train_time == main_results$test_time) == dim(main_results)[1]) || plot_type == 'line') {
    
    # if only trained and tested at the same time, create line plot
    main_results %>%
      dplyr::filter(train_time == test_time) %>%
      ggplot(aes(test_time, accuracy)) +
      geom_line() +
      facet_wrap(~result_type, scales = "free") + 
      xlab('Time') + 
      ylab('Accuracy')

    
  } else {
    
    
    # should come up with something better so that the fill colors can be on different scales
    
    # if trained and testing at all times, create a TCD plot
    main_results %>%
      ggplot(aes(test_time, train_time, fill = accuracy)) + 
      geom_tile() +
      facet_wrap(~result_type) +
      scale_fill_continuous(type = "viridis", name = "Prediction \n accuracy") +
      ylab('Train time') + 
      xlab('Test time') 
    
  }
  
}




get_parameters.rm_main_results = function(confusion_matrix_obj){
  
  data.frame(rm_main_results.rm_main_results = "does not have settable parameters")

}






