



# the constructor 
#' @export
main_results_RM <- function(the_data = data.frame(), state = 'initial', options = NULL){
  
  main_results_obj <- the_data
  
  attr(main_results_obj, "class") <- c("main_results_RM", 'data.frame')
  
  attr(main_results_obj, "state") <- state

  attr(main_results_obj, "options") <- options
  
  main_results_obj
  
}





# aggregate the results from all the cross-validation splits
#' @export
aggregate_CV_split_results.main_results_RM = function(main_results_obj, prediction_results) {
  
  
  # perhaps include a warning if the state is not intial
  if (attr(main_results_obj, "state") != "initial") {    
    warning(paste0("The method aggregate_CV_split_results() should only be called on",
                   "main_results_RM that are in the intial state.",
                   "Any data that was already stored in this object will be overwritten"))
  }

  
  decision_vals <- select(prediction_results, starts_with("decision"))
  num_classes <- ncol(decision_vals)
  num_test_points <- nrow(decision_vals)
  
  
  # remove the prefix 'decision_vals' from the column names...
  the_names <- names(decision_vals)
  the_names <- unlist(strsplit(the_names, "decision_val_", fixed = TRUE))
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

  
  main_results_RM(the_results, 'results combined over one cross-validation split', attributes(main_results_obj)$options)
  
}




# aggregate the results from all the resample runs
#' @export
aggregate_resample_run_results.main_results_RM = function(resample_run_results) {
  
  
  central_results <- resample_run_results %>%                 
    group_by(train_time, test_time) %>%
    summarize(zero_one_loss = mean(zero_one_loss),
              normalized_rank = mean(normalized_rank),
              decision_vals = mean(decision_vals))
  
  
  main_results_RM(central_results, 'final results', attributes(resample_run_results)$options)
  
  
}



# plot results (TCT plot for now)
#' @export
plot.main_results_RM = function(central_results) {
  
  # will need to come up with something better so that the fill colors can be on different scales
  central_results %>%
    tidyr::gather(result_type, accuracy, -train_time, -test_time) %>%
    ggplot(aes(test_time, train_time, fill = accuracy)) + 
    geom_tile() +
    facet_wrap(~result_type)
  
  
  # line plots of the results
  # central_results %>%
  #   filter(train_time == test_time) %>%
  #   tidyr::gather(result_type, accuracy, -train_time, -test_time) %>%
  #   ggplot(aes(test_time, accuracy)) + 
  #   geom_point() + 
  #   facet_wrap(~result_type, scales = "free")
  
  
}


