#' A result metric (RM) that calculates main decoding accuracy measures
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
#' @param aggregate_decision_values A string or boolean specifying how the
#'  decision values should be aggregated. If this is a boolean
#'  set to TRUE or to the string "full", then the decision values for the correct 
#'  category will be calculated. If this is a boolean set to FALSE or to the 
#'  string "none", then the decision values will not be calculated. If this is a
#'  string set to either "diag" or "only same train test time" then the decision
#'  values will only be calculated when for results when training and testing
#'  at the same time. Not returning the full results can speed up the runtime
#'  of the code and will use less memory so this can be useful for large data sets.
#' 
#' @param aggregate_normalized_rank A string or boolean specifying how the
#'  normalized rank results should be aggregated. If this is a boolean
#'  set to TRUE or to the string "full", then the decision values for the correct 
#'  category will be calculated. If this is a boolean set to FALSE or to the 
#'  string "none", then the decision values will not be calculated. If this is a
#'  string set to either "diag" or "only same train test time" then the decision
#'  values will only be calculated when for results when training and testing
#'  at the same time. Not returning the full results can grealy speed up the runtime
#'  of the code and will use less memory so this can be useful for large data sets.
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
rm_main_results <- function(aggregate_decision_values = TRUE, aggregate_normalized_rank = TRUE){
  
  options <- list(aggregate_decision_values = aggregate_decision_values, 
                  aggregate_normalized_rank = aggregate_normalized_rank)
  
  rm_obj <- new_rm_main_results(options = options)
  
  rm_obj

}


# the internal private constructor
new_rm_main_results <- function(the_data = data.frame(), state = 'initial', options = NULL){
  
  rm_obj <- the_data
  
  attr(rm_obj, "class") <- c("rm_main_results", 'data.frame')
  
  attr(rm_obj, "state") <- state
  
  attr(rm_obj, "options") <- options
  
  rm_obj
  
}






# The aggregate_CV_split_results method needed to fulfill the results metric interface
#' @export
aggregate_CV_split_results.rm_main_results = function(rm_obj, prediction_results) {
  
  
  # perhaps include a warning if the state is not intial
  if (attr(rm_obj, "state") != "initial") {    
    warning(paste0("The method aggregate_CV_split_results() should only be called on",
                   "rm_main_results that are in the intial state.",
                   "Any data that was already stored in this object will be overwritten"))
  }

  
  # get the options for now the normalized rank and decision values should be aggregated
  aggregate_decision_values <- attr(rm_obj, "options")$aggregate_decision_values
  aggregate_normalized_rank <- attr(rm_obj, "options")$aggregate_normalized_rank
  
  
  # get the zero-one loss loss results
  the_results <- prediction_results %>%
    dplyr::mutate(correct = .data$actual_labels == .data$predicted_labels) %>%
    dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$correct, na.rm = TRUE))
  

  
  # get the normalized rank decision values
  if (aggregate_normalized_rank != FALSE && aggregate_normalized_rank != "none") {
    
    # data slightly augmented version of that has actual_labels with decision_vals. appended
    prediction_results_aug <- get_augmented_prediction_results(prediction_results, aggregate_normalized_rank)
    decision_vals_aug <- select(prediction_results_aug, starts_with("decision"))
    decision_vals_rest <- select(prediction_results_aug, -starts_with("decision"))
    
    # get the normalized rank decision values
    get_rank_one_row <- function(decision_vals_aug_row) {
      actual_label <- decision_vals_aug_row[1]
      decision_vals_row <- decision_vals_aug_row[2:length(decision_vals_aug_row)]
      the_names <- names(decision_vals_row)
      the_order <- order(as.numeric(decision_vals_row), decreasing = TRUE)
      which(the_names[the_order] == actual_label)
    }
    
    
    num_classes <- prediction_results %>% select(starts_with("decision_vals")) %>% ncol(.)
    
    normalized_rank_results <- 1 - ((apply(decision_vals_aug, 1, get_rank_one_row) - 1)/(num_classes - 1))
    
    summarized_normalized_rank_results <- decision_vals_rest %>%
      mutate(normalized_rank = normalized_rank_results) %>%
      dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
      summarize(normalized_rank = mean(.data$normalized_rank, na.rm = TRUE))
    
    the_results <- left_join(the_results, summarized_normalized_rank_results, 
                             by = c("CV", "train_time", "test_time"))
    
  }
    

  
  
  
  # get the decision values for the correct label
  if (aggregate_decision_values != FALSE && aggregate_decision_values != "none") {
    
    prediction_results_aug <- get_augmented_prediction_results(prediction_results, aggregate_decision_values)
    decision_vals_aug <- select(prediction_results_aug, starts_with("decision"))
    decision_vals_rest <- select(prediction_results_aug, -starts_with("decision"))
    
    get_decision_vals_one_row <- function(decision_vals_aug_row) {
      decision_vals_aug_row[which(as.character(as.matrix(decision_vals_aug_row[1])) == names(decision_vals_aug_row[2:length(decision_vals_aug_row)])) + 1]
    }
    
    correct_class_decision_val <- as.numeric(apply(decision_vals_aug, 1, get_decision_vals_one_row))
  
    summarized_correct_decision_val_results <- decision_vals_rest %>%
      mutate(decision_vals = correct_class_decision_val) %>%
      dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
      summarize(decision_vals = mean(.data$decision_vals, na.rm = TRUE))
    
    the_results <- left_join(the_results, summarized_correct_decision_val_results, 
                             by = c("CV", "train_time", "test_time"))
  
  }
  

  new_rm_main_results(the_results, 
                      'results combined over one cross-validation split', 
                      attributes(rm_obj)$options)
  
}




# The aggregate_resample_run_results method needed to fulfill the results metric interface
#' @export
aggregate_resample_run_results.rm_main_results = function(resample_run_results) {
  
  
  central_results <- resample_run_results %>%                 
    group_by(.data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$zero_one_loss))
  
  
  if ("normalized_rank" %in% names(resample_run_results)) {
    
    normalized_rank_results <- resample_run_results %>%                 
      group_by(.data$train_time, .data$test_time) %>%
      summarize(normalized_rank = mean(.data$normalized_rank))
    
    central_results <- left_join(central_results, normalized_rank_results, by = c("train_time", "test_time"))
    
  }
    
    
  if ("decision_vals" %in% names(resample_run_results)) {
    
    decision_vals_results <- resample_run_results %>%                 
      group_by(.data$train_time, .data$test_time) %>%
      summarize(decision_vals = mean(.data$decision_vals))
    
    central_results <- left_join(central_results, decision_vals_results, by = c("train_time", "test_time"))
    
  }
              

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
  
  
  # convert the zero-one loss results to percentages
  main_results <- dplyr::mutate(main_results, zero_one_loss = zero_one_loss * 100)
  
  
  if (result_type == 'all'){ 
    # do nothing
  } else if (result_type == 'zero_one_loss'){
    main_results <- dplyr::select(main_results, train_time, test_time, zero_one_loss)
  } else if (result_type == 'normalized_rank'){
    if (!(result_type %in% main_results)){ 
      stop(paste("Can't plot", result_type, "results because this type of result was not saved."))}
    main_results <- dplyr::select(main_results, train_time, test_time, normalized_rank)
  } else if (result_type == 'decision_vals'){
    if (!(result_type %in% main_results)){ 
      stop(paste("Can't plot", result_type, "results because this type of result was not saved."))}
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
  
  
  main_results <- main_results %>%
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



#' @export
get_parameters.rm_main_results = function(ndtr_obj){
  
  # get the options for now the normalized rank and decision values should be aggregated
  aggregate_decision_values <- attr(ndtr_obj, "options")$aggregate_decision_values
  aggregate_normalized_rank <- attr(ndtr_obj, "options")$aggregate_normalized_rank
  
  data.frame(rm_main_results.aggregate_decision_values = aggregate_decision_values,
             rm_main_results.aggregate_normalized_rank = aggregate_normalized_rank)
  
}





# private helper function to get data needed to create the normalized rank 
#  and decision value results
get_augmented_prediction_results <- function(prediction_results, aggregate_options) {
  
  # if only getting the decision values when training and testing at the same time
  if (aggregate_options == "diag" || aggregate_options == "only same train test time"){
    
    prediction_results <- prediction_results %>%
      filter(train_time == test_time)
    
    # if getting the decision values for all time points    
  } else if (aggregate_options == TRUE || aggregate_options == "full") {
    
      # if getting data from all times do nothing
    
  } else {
    
    argument_name <- deparse(substitute(aggregate_options)) 
    stop(paste0(argument_name, " was set to ", aggregate_options, ". ", argument_name, 
                " must be set to one of the following: ",
                "TRUE, 'all', FALSE, 'none', 'diag', or 'only same train test time'"))
  }
  
  
  # add decision_vals. to the actual label names to allow a comparion
  #  of the actual labels to column names
  prediction_results <- prediction_results %>%
    mutate(decision_actual_labels = paste0("decision_vals.", actual_labels)) %>%
    select(decision_actual_labels, starts_with("decision"), everything())
  
  prediction_results
    
}







