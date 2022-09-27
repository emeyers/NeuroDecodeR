#' A result metric (RM) that calculates confusion matrices
#'
#' This result metric calculate a confusion matrices from all points in time.
#'
#' @details
#' Like all result metrics, this result metric has functions to aggregate
#' results after completing each set of cross-validation classifications, and
#' also after completing all the resample runs. The results should then be
#' available in the DECODING_RESULTS object returned by the cross-validator.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the rm_confusion_matrix feature preprocessor work with the
#'   magrittr pipe (%>%) operator. This argument should almost never be directly
#'   set by the user to anything other than NULL. If this is set to the default
#'   value of NULL, then the constructor will return a rm_confusion_matrix
#'   object. If this is set to an ndr container, then a rm_confusion_matrix
#'   object will be added to the container and the container will be returned.
#'   If this argument is set to another ndr object, then both that ndr object as
#'   well as a new rm_confusion_matrix object will be added to a new container
#'   and the container will be returned.
#'
#' @param save_TCD_results A Boolean specifying whether one wants
#'   to save results to allow one to create temporal cross decoding confusion
#'   matrices; i.e., confusion matrices when training at one point in time and
#'   testing a different point in time. Setting this to FALSE can save memory.
#'
#' @param create_decision_vals_confusion_matrix A boolean specifying whether one
#'   wants to create a confusion matrix of the decision values. In this
#'   confusion matrix, each row corresponds to the correct class (like a regular
#'   confusion matrix) and each column corresponds to the mean decision value of
#'   the predictions for each class.
#'   
#' @return This constructor creates an NDR result metric object with the class
#'   `rm_confusion_matrix`. Like all NDR result metric objects, this result
#'   metric will be used by a cross-validator to create a measure of decoding
#'   accuracy by aggregating the results after all cross-validation splits have
#'   been run, and after all resample runs have completed.
#'
#' @examples
#' # If you only want to use the rm_confusion_matrix(), then you can put it in a
#' # list by itself and pass it to the cross-validator.
#' the_rms <- list(rm_confusion_matrix())
#'
#'
#' @family result_metrics
#'
#'
#'
#' @export
rm_confusion_matrix <- function(ndr_container_or_object = NULL, 
                                save_TCD_results = FALSE,
                                create_decision_vals_confusion_matrix = TRUE) {
  options <- list(
    save_TCD_results = save_TCD_results,
    create_decision_vals_confusion_matrix = create_decision_vals_confusion_matrix)

  rm_obj <- new_rm_confusion_matrix(data.frame(), "initial", options)
  
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the result metric in it
  put_ndr_object_in_container(ndr_container_or_object, rm_obj)
  
}





# the internal constructor
new_rm_confusion_matrix <- function(the_data = data.frame(),
                                    the_state = NULL,
                                    options = NULL) {
  rm_obj <- the_data
  attr(rm_obj, "state") <- the_state
  attr(rm_obj, "options") <- options
  attr(rm_obj, "class") <- c("rm_confusion_matrix", "data.frame")

  rm_obj
  
}





# The aggregate_CV_split_results method needed to fulfill the results metric interface.
# Not going to export this since it should never be directly called by users of the NDR.
#' @inherit aggregate_CV_split_results
#' @keywords internal
aggregate_CV_split_results.rm_confusion_matrix <- function(rm_obj, prediction_results) {

  # include a warning if the state is not initial
  if (attr(rm_obj, "state") != "initial") {
    warning(paste0(
      "The method aggregate_CV_split_results() should only be called on",
      "rm_confusion_matrix that is in the intial state.",
      "Any data that was already stored in this object will be overwritten."))
  }



  # If specified in the constructor, save the confusion matrix only for training and testing
  # the same times. This will save memory, and the off diagonal element confusion matrices
  # won't generally be of too much interest - however they could be of interest for
  # computing a TCD plot of mutual information.

  options <- attr(rm_obj, "options")

  if (!options$save_TCD_results) {
    prediction_results <- prediction_results %>%
      dplyr::filter(.data$train_time == .data$test_time)
  }


  # create the confusion matrix
  confusion_matrix <- prediction_results %>%
    dplyr::group_by(.data$train_time, .data$test_time, .data$actual_labels, .data$predicted_labels) %>%
    summarize(n = n())


  if (options$create_decision_vals_confusion_matrix) {

    # create the decision value confusion matrix
    confusion_matrix_decision_vals <- prediction_results %>%
      select(-.data$predicted_labels, -.data$CV) %>%
      dplyr::group_by(.data$train_time, .data$test_time, .data$actual_labels) %>%
      summarize_all(mean) %>%
      tidyr::pivot_longer(starts_with("decision_vals"),
        names_to = "predicted_labels",
        values_to = "mean_decision_vals") %>%
      mutate(predicted_labels = gsub("decision_vals.", "", .data$predicted_labels))

    confusion_matrix <- left_join(confusion_matrix_decision_vals, confusion_matrix,
      by = c("train_time", "test_time", "actual_labels", "predicted_labels"))

    confusion_matrix$n[is.na(confusion_matrix$n)] <- 0
    
  }


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


  new_rm_confusion_matrix(
    confusion_matrix,
    "results combined over one cross-validation split",
    attr(rm_obj, "options"))
  
}







# The aggregate_resample_run_results method needed to fulfill the results metric interface.
# Not going to export this since it should never be directly called by users of the NDR.
#' @inherit aggregate_resample_run_results
#' @keywords internal
aggregate_resample_run_results.rm_confusion_matrix <- function(resample_run_results) {
  
  confusion_matrix <- resample_run_results

  # add on 0's for all entries in the confusion matrix that are missing

  # check if only specified that one should only save the results at the same
  # training and test time or if the results only were recorded for the same
  # train and test times (since this was specied in the CV obj)
  options <- attr(resample_run_results, "options")
  only_has_same_train_test_time_results <-
    (sum(resample_run_results$train_time == resample_run_results$test_time) == dim(resample_run_results)[1])

  if (!options$save_TCD_results || only_has_same_train_test_time_results) {

    # create smaller matrix of 0's if only saving results of training and testing at the same time
    cm_label_matrix <- expand.grid(
      actual_labels = unique(confusion_matrix$actual_labels),
      predicted_labels = unique(confusion_matrix$predicted_labels))

    time_matrix <- data.frame(
      train_time = unique(confusion_matrix$train_time),
      test_time = unique(confusion_matrix$test_time))

    empty_cm <- data.frame(
      resample_run = "0",
      time_matrix[rep(1:dim(time_matrix)[1], dim(cm_label_matrix)[1]), ],
      cm_label_matrix[rep(1:dim(cm_label_matrix)[1], each = dim(time_matrix)[1]), ],
      n = 0L)

    # could just filter the results below using train_time == test_time but this would require a lot more memory
  } else {
    
    empty_cm <- expand.grid(
      resample_run = "0",
      train_time = unique(confusion_matrix$train_time),
      test_time = unique(confusion_matrix$test_time),
      actual_labels = unique(confusion_matrix$actual_labels),
      predicted_labels = unique(confusion_matrix$predicted_labels),
      n = 0L, stringsAsFactors = FALSE)
    
  }



  confusion_matrix <- dplyr::bind_rows(confusion_matrix, empty_cm)


  # calculate the final confusion matrix
  confusion_matrix <- confusion_matrix %>%
    dplyr::group_by(.data$train_time, .data$test_time, .data$actual_labels, .data$predicted_labels) %>%
    summarize(n = sum(n)) %>%
    dplyr::group_by(.data$train_time, .data$test_time, .data$actual_labels) %>%
    mutate(conditional_pred_freq = n / sum(n)) # Pr(predicted = y | actual = k)


  if (options$create_decision_vals_confusion_matrix) {
    
    confusion_matrix_decision_vals <- resample_run_results %>%
      dplyr::group_by(.data$train_time, .data$test_time, .data$actual_labels, .data$predicted_labels) %>%
      summarize(mean_decision_vals = mean(.data$mean_decision_vals))

    if (dim(confusion_matrix_decision_vals)[1] != dim(confusion_matrix)[1]) {
      warning(paste('Something has gone wrong where the confusion_matrix_decision_vals and",
                    "confusion_matrix do not have the same number of rows'))
    }

    confusion_matrix <- confusion_matrix %>%
      left_join(confusion_matrix_decision_vals,
        by = c("train_time", "test_time", "actual_labels", "predicted_labels"))
    
  }


  new_rm_confusion_matrix(
    confusion_matrix,
    "final results",
    attr(resample_run_results, "options"))
  
}






#' A plot function for the rm_confusion_matrix object
#'
#' This function plots confusion matrices after the decoding analysis has been
#' run (and all results have been aggregated). This function can also plot
#' mutual information calculated from the confusion matrix.
#'
#' @param x A rm_confusion_matrix object that has aggregated runs from a
#'   decoding analysis, e.g., if DECODING_RESULTS are the output from the
#'   run_decoding(cv) then this argument should be
#'   `DECODING_RESULTS$rm_confusion_matrix`.
#'
#' @param ... This is needed to conform to the plot generic interface.
#'
#' @param results_to_show A string specifying the type of result to plot that can
#'   take the following values:
#'   * "zero_one_loss": plot a regular confusion matrix.
#'   * "decision_vals": plot a confusion matrix with the average decision values.
#'   * "mutual_information": plot the mutual information calculated from the
#'   zero-one loss confusion matrix.
#'
#' @param plot_TCD_results A Boolean indicating whether the
#'   a cross-temporal decoding of the confusion matrices should only be plotted. 
#'   If the `results_to_show == "mutual_information"` setting this to TRUE
#'   will plot a TCD plot of the mutual information otherwise it will plot a
#'   line plot of the mutual information for training and testing at the same
#'   time.
#'   
#' @param plot_only_one_train_time If this is set to a numeric value the the
#'   confusion matrix will only be plotted for the training time *start time*
#'   that is specified. If the number passed is not equal to an exact start
#'   training time, then the closest training time will be used and a message
#'   saying that the time specified does not exist will be printed.
#'   
#' @return Returns a ggplot object that plots the confusion matrix results.   
#'   
#' @family result_metrics
#'
#'
#' @export
plot.rm_confusion_matrix <- function(x, ..., results_to_show = "zero_one_loss",
                                     plot_TCD_results = FALSE,
                                     plot_only_one_train_time = NULL) {
  
  
  saved_only_at_same_train_test_time <- !attr(x, "options")$save_TCD_results


  if ((saved_only_at_same_train_test_time) && plot_TCD_results == TRUE) {
    
    warning(paste(
      "Options are set to plot at all times (plot_TCD_results = TRUE)",
      "but the cross temporal decoding results were not saved.",
      "To plot the results for training and testing at all times you need to set",
      "rm_confusion_matrix(save_TCD_results = TRUE) in the",
      "rm_confusion_matrix constructor prior to running the decoding analysis."))
  }


  # if results_to_show is "zero_one_loss" or "decision_vals" plot a confusion matrix
  if ((results_to_show == "zero_one_loss") || (results_to_show == "decision_vals")) {
    
    should_decision_vals_cm <- results_to_show == "decision_vals"
    plot_confusion_matrix(x, 
                          plot_TCD_results, 
                          plot_only_one_train_time, 
                          should_decision_vals_cm)

    # otherwise plot mutual information calculated from zero-one loss confusion matrix
  } else if (results_to_show == "mutual_information") {
    
    if (!plot_TCD_results) {
      plot_type <- "line"
    } else {
      plot_type <- "TCD"
    }

    plot_MI(x, plot_type)
    
  } else {
    
    stop("results_to_show must be set to the value of 'zero_one_loss', 'decision_vals' or
         'mutual_information'")
  }
  

}




# a private function to plot the confusion matrix
plot_confusion_matrix <- function(confusion_matrix_obj,
                                  plot_TCD_results = FALSE,
                                  plot_only_one_train_time = NULL,
                                  plot_decision_vals_confusion_matrix = FALSE) {


  # should perhaps give an option to choose a different color scale, and maybe other options?

  # checking if only have the results for training and testing at the same time
  # could look at the 'options' attribute for this, but that won't help if the filtering happened at the
  # level of the cross-validator
  only_has_same_train_test_time_results <-
    (sum(confusion_matrix_obj$train_time == confusion_matrix_obj$test_time) == dim(confusion_matrix_obj)[1])

  confusion_matrix_obj$train_time <- round(get_center_bin_time(confusion_matrix_obj$train_time))
  confusion_matrix_obj$test_time <- round(get_center_bin_time(confusion_matrix_obj$test_time))

  # confusion_matrix_obj$train_time <- get_time_range_strings(confusion_matrix_obj$train_time)
  # confusion_matrix_obj$test_time <- get_time_range_strings(confusion_matrix_obj$test_time)

  # if only want the results plotted for the same training and test times
  if (!only_has_same_train_test_time_results && !plot_TCD_results) {
    confusion_matrix_obj <- confusion_matrix_obj %>%
      filter(.data$train_time == .data$test_time)
  }
  
  
  if (!is.null(plot_only_one_train_time)) {
    
   if (!is.numeric(plot_only_one_train_time)) {
     stop("The plot_only_one_train_time argument must be NULL or set to a numeric value.")
   }
    
    
    the_train_times <- unique(confusion_matrix_obj$train_time)
    
    if (sum(plot_only_one_train_time == the_train_times) == 0) {
      
      closest_time <- the_train_times[which.min(abs(plot_only_one_train_time - the_train_times))]
      
      
      message(paste0("The plot_only_one_train_time argument value of ", plot_only_one_train_time,
                     " does not match any of the rm_confusion_matrix train times. ",
                     "Selecting the closest starting training time available which is ", closest_time, "."))
      
      plot_only_one_train_time <- closest_time
      
    }
    
    
    confusion_matrix_obj <- confusion_matrix_obj %>%
      filter(.data$train_time == plot_only_one_train_time)
    
     
  }
  
  
  


  if (FALSE) { # (only_has_same_train_test_time_results) {

    # Add the word 'Time' to the title since there is enough space to plot it
    # when only training and testing at the same time

    train_time_order <- paste("Time", unique(sort(confusion_matrix_obj$train_time)))
    confusion_matrix_obj$train_time <- ordered(
      paste("Time", confusion_matrix_obj$train_time),
      levels = train_time_order)

    test_time_order <- paste("Time", unique(sort(confusion_matrix_obj$test_time)))
    confusion_matrix_obj$test_time <- ordered(
      paste("Time", confusion_matrix_obj$test_time),
      levels = test_time_order)
    
  }


  if (plot_decision_vals_confusion_matrix) {
    
    g <- confusion_matrix_obj %>%
      ggplot(aes(.data$predicted_labels, forcats::fct_rev(.data$actual_labels), fill = .data$mean_decision_vals)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("True class") +
      xlab("Predicted class") + # or should I transpose this (people do it differently...)
      scale_fill_continuous(type = "viridis", name = "Mean\n decision\n value") #+ 
    
  } else {
    
    g <- confusion_matrix_obj %>%
      ggplot(aes(.data$predicted_labels, forcats::fct_rev(.data$actual_labels), fill = .data$conditional_pred_freq * 100)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("True class") +
      xlab("Predicted class") + # or should I transpose this (people do it differently...)
      scale_fill_continuous(type = "viridis", name = "Prediction\n frequency") #+ 
    
  }


  if (sum(confusion_matrix_obj$train_time == confusion_matrix_obj$test_time) == dim(confusion_matrix_obj)[1]) {
    g + facet_wrap(~ .data$train_time)
  } else {
    g + facet_grid(.data$train_time ~ .data$test_time)
  }
  
  
}





# a private helper function to calculate and plot mutual information from the confusion matrix
plot_MI <- function(rm_obj, plot_type = "TCD") {
  
  if (!(plot_type == "TCD" || plot_type == "line")) {
    warning("plot_type must be set to 'TCD' or 'line'. Using the default value of 'TCD'")
  }


  # calculate the mutual information ------------------------------------------

  MI_obj <- rm_obj %>%
    group_by(.data$train_time, .data$test_time) %>%
    mutate(joint_probability = n / sum(n)) %>%
    group_by(.data$train_time, .data$test_time, .data$actual_labels) %>%
    mutate(log_marginal_actual = log2(sum(.data$joint_probability))) %>%
    group_by(.data$train_time, .data$test_time, .data$predicted_labels) %>%
    mutate(log_marginal_predicted = log2(sum(.data$joint_probability))) %>%
    ungroup() %>%
    mutate(log_joint_probability = log2(.data$joint_probability)) %>%
    mutate(log_joint_probability = replace(.data$log_joint_probability, .data$log_joint_probability == -Inf, 0)) %>%
    mutate(MI_piece = .data$joint_probability * (.data$log_joint_probability - .data$log_marginal_actual - .data$log_marginal_predicted)) %>%
    group_by(.data$train_time, .data$test_time) %>%
    summarize(MI = sum(.data$MI_piece))


  # plot the mutual information  ----------------------------------------------

  MI_obj$train_time <- round(get_center_bin_time(MI_obj$train_time))
  MI_obj$test_time <- round(get_center_bin_time(MI_obj$test_time))


  if ((sum(MI_obj$train_time == MI_obj$test_time) == dim(MI_obj)[1]) || plot_type == "line") {

    # if only trained and tested at the same time, create line plot
    MI_obj %>%
      dplyr::filter(.data$train_time == .data$test_time) %>%
      ggplot(aes(.data$test_time, .data$MI)) +
      geom_line() +
      xlab("Time") +
      ylab("Mutual information (bits)") # +
    # geom_hline(yintercept = 0, color = "red")  # how much MI there should be if there is no bias
    
  } else {
    
    MI_obj %>%
      ggplot(aes(.data$test_time, .data$train_time, fill = .data$MI)) +
      geom_tile() +
      ylab("Test time") +
      xlab("Train time") +
      scale_fill_continuous(type = "viridis", name = "Bits") +
      ggtitle("Mutual information") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  
}



# Returns the parameters that were set in the rm_confusion_matrix object
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.rm_confusion_matrix <- function(ndr_obj) {

  # there is only one parameter option that can be set here so return it
  data.frame(
    rm_confusion_matrix.save_TCD_results =
      attributes(ndr_obj)$options$save_TCD_results,
    rm_confusion_matrix.create_decision_vals_confusion_matrix =
      attributes(ndr_obj)$options$create_decision_vals_confusion_matrix)
  
}
