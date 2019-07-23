#' A result metric (RM) that calculates central decoding accuracy measures
#'
#' This result metric calculate the zero-one loss, the normalized rank, and the
#'  mean of the decision values. This is also an S3 object which has an associated
#'  plot function to display the results.
#' 
#' @details
#' Like all result metrics, this result metric has functions to aggregregate
#' results after completing each set of cross-validation classifications, and
#' also after completing all the resample runs. The results should then be
#' available in the DECODING_RESULTS object returned by the cross-validator.
#'
#'
#' @examples
#' # This result metric does not take any arguments.
#' # If you only want to use the rm_main_results(), then you can put it in a
#' # list by itself and pass it to the cross-validator.
#' the_rms <- list(rm_main_results())
#' 
#' @family result_metrics





# the constructor 
#' @export
rm_main_results <- function(){
  
  main_results_obj <- new_rm_main_results()
  
  main_results_obj

}


# the internal private constructor
new_rm_main_results <- function(the_data = data.frame(), state = 'initial', options = NULL){
  
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
     dplyr::mutate(correct = .data$actual_labels == .data$predicted_labels)
  
  
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
    dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$correct),
              normalized_rank = mean(.data$normalized_rank_results),
              decision_vals = mean(.data$decision_values))

  
  new_rm_main_results(the_results, 
                      'results combined over one cross-validation split', 
                      attributes(main_results_obj)$options)
  
}




# aggregate the results from all the resample runs
#' @export
aggregate_resample_run_results.rm_main_results = function(resample_run_results) {
  
  
  central_results <- resample_run_results %>%                 
    group_by(.data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$zero_one_loss),
              normalized_rank = mean(.data$normalized_rank),
              decision_vals = mean(.data$decision_vals))
  
  
 new_rm_main_results(central_results, 
                     'final results', 
                     attributes(resample_run_results)$options)
  
}





#' A plot function for the rm_main_results object
#'
#' This function can plot line results or temporal cross-decoding results for
#' the the zero-one loss, normalized rank and/or decision values after the
#' decoding analysis has been run (and all results have been aggregated)
#' 
#' @param main_results A rm_main_result object that has aggregated runs from a
#'   decoding analysis, e.g., if DECODING_RESULTS are the out from the
#'   run_decoding(cv) then this argument should be
#'   DECODING_RESULTS$rm_main_results.
#' 
#' @param result_type A string specifying the types of results to plot options
#'   are: 'zero_one_loss', 'normalized_rank', 'decision_values', or 'all'
#' 
#' @param plot_type A string specifying the type of results to plot. Options are
#'   'TCD' to plot a temporal cross decoding matrix or 'line' to create a line
#'   plot of the decoding results as a function of time
#' 
#' @family result_metrics
#' 
#' @export
plot.rm_main_results = function(main_results, result_type = 'zero_one_loss', plot_type = 'TCD') {
  
  
  if (attributes(main_results)$state != "final results"){
    stop("The results can only be plotted *after* the decoding analysis has been run")
  }
  
  
  if (result_type == 'all'){ 
    # do nothing
  } else if (result_type == 'zero_one_loss'){
    main_results <- dplyr::select(main_results, train_time, test_time, zero_one_loss)
  } else if (result_type == 'normalized_rank'){
    main_results <- dplyr::select(main_results, train_time, test_time, normalized_rank)
  } else if (result_type == 'decision_vals'){
    main_results <- dplyr::select(main_results, train_time, test_time, decision_vals)
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
  

  # if only a single time, just plot a bar for the decoding accuracy
  if (length(unique(main_results$train_time)) == 1){
    
    main_results %>%
      ggplot(aes(test_time, accuracy)) +
      geom_col() +
      facet_wrap(~result_type, scales = "free") + 
      xlab('Time') + 
      ylab('Accuracy')
    

  } else if ((sum(main_results$train_time == main_results$test_time) == dim(main_results)[1]) || 
             plot_type == 'line') {
    
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






