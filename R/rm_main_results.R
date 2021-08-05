#' A result metric (RM) that calculates main decoding accuracy measures
#'
#' This result metric calculate the zero-one loss, the normalized rank, and the
#' mean of the decision values. This is also an S3 object which has an
#' associated plot function to display the results.
#'
#' @details
#' Like all result metrics, this result metric has functions to aggregregate
#' results after completing each set of cross-validation classifications, and
#' also after completing all the resample runs. The results should then be
#' available in the DECODING_RESULTS object returned by the cross-validator.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the rm_main_results feature preprocessor work with the
#'   magrittr pipe (%>%) operator. This argument should almost never be directly
#'   set by the user to anything other than NULL. If this is set to the default
#'   value of NULL, then the constructor will return a rm_main_results object.
#'   If this is set to an ndr container, then a rm_main_results object will be
#'   added to the container and the container will be returned. If this argument
#'   is set to another ndr object, then both that ndr object as well as a new
#'   rm_main_results object will be added to a new container and the container
#'   will be returned.
#'
#' @param include_norm_rank_results An argument specifying if the normalized
#'   rank and decision value results should be saved. If this is a Boolean set
#'   to TRUE, then the normalized rank and decision values for the correct
#'   category will be calculated. If this is a Boolean set to FALSE then the
#'   normalized rank and decision values will not be calculated. If this is a
#'   string set to "only_same_train_test_time", then the normalized rank and decision
#'   values will only be calculated when for results when training and testing
#'   at the same time. Not returning the full results can speed up the run-time
#'   of the code and will use less memory so this can be useful for large data
#'   sets.
#'
#'
#' @examples
#' # If you only want to use the rm_main_results(), then you can put it in a
#' # list by itself and pass it to the cross-validator.
#' the_rms <- list(rm_main_results())
#' 
#' 
#' @family result_metrics
#'
#'
#'
#' @export
rm_main_results <- function(ndr_container_or_object = NULL, 
                            include_norm_rank_results = TRUE) {
  
  options <- list(include_norm_rank_results = include_norm_rank_results)

  rm_obj <- new_rm_main_results(options = options)
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the result metric in it
  put_ndr_object_in_container(ndr_container_or_object, rm_obj)
  
}




# the internal private constructor
new_rm_main_results <- function(the_data = data.frame(), state = "initial", options = NULL) {
  
  rm_obj <- the_data

  attr(rm_obj, "class") <- c("rm_main_results", "data.frame")
  attr(rm_obj, "state") <- state
  attr(rm_obj, "options") <- options

  rm_obj
  
}





# The aggregate_CV_split_results method needed to fulfill the results metric interface.
aggregate_CV_split_results.rm_main_results <- function(rm_obj, prediction_results) {


  # return a warning if the state is not initial
  if (attr(rm_obj, "state") != "initial") {
    
    warning(paste0(
      "The method aggregate_CV_split_results() should only be called on",
      "rm_main_results that are in the intial state.",
      "Any data that was already stored in this object will be overwritten"))
    
  }


  # get the options for how the normalized rank and decision values should be aggregated
  include_norm_rank_results <- attr(rm_obj, "options")$include_norm_rank_results


  # get the zero-one loss loss results
  the_results <- prediction_results %>%
    dplyr::mutate(correct = .data$actual_labels == .data$predicted_labels) %>%
    dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$correct, na.rm = TRUE))

  
  # calculate the chance level for the zero one loss results
  zero_one_loss_chance_level <- 1/length(unique(prediction_results$actual_labels))
  
  
  # newly added code to speed things up
  if (include_norm_rank_results != FALSE) {
      
    
    # check for invalid include_norm_rank_results argument values
    if (!((include_norm_rank_results == TRUE) || (include_norm_rank_results == 'only_same_train_test_time'))) {
    
      stop(paste0("'include_norm_rank_results' argument was set to ", include_norm_rank_results, ". ", 
                  "'include_norm_rank_results' must be set to one of the following: TRUE, FALSE, or 'only_same_train_test_time'"))
    }

  
    # if "only_same_train_test_time" option is selected only use data for training and testing at the same time
    if (include_norm_rank_results == "only_same_train_test_time") {
      prediction_results <- prediction_results %>%
        dplyr::filter(.data$train_time == .data$test_time)
    }
    
    
    actual_labels <- paste0("decision_vals.", prediction_results$actual_labels)  
    data_matrix <- dplyr::select(prediction_results, starts_with('decision_vals'))
    col_names <- names(data_matrix)
    data_matrix <- as.matrix(data_matrix)
    
    
    # Why are there NAs in the decision values? 
    # Perhaps because all values were the same when doing the correlation? 
    # Could use the below code to remove them, but better to remove them at the classification stage.
    # data_matrix[is.na(data_matrix)] <- 0
    

    # get the indices of the column that each actual label corresponds to
    get_actual_label_ind <- function(actual_label) {
      which(actual_label == col_names)
    }
    actual_label_inds <- sapply(actual_labels, get_actual_label_ind)
    
    
    correct_class_decision_val <- data_matrix[matrix(c(seq_along(actual_label_inds), actual_label_inds), 
                                                     nrow = length(actual_label_inds))]
    
    
    
    # add the decision values to the results
    summarized_correct_decision_val_results <- prediction_results %>%
      select(-starts_with("decision")) %>%
      mutate(decision_vals = correct_class_decision_val) %>%
      dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
      summarize(decision_vals = mean(.data$decision_vals, na.rm = TRUE))
      
      the_results <- left_join(the_results, summarized_correct_decision_val_results,
                               by = c("CV", "train_time", "test_time"))
      



    # get the normalized rank results...
      diff_decision_vals <- sweep(data_matrix, 1, correct_class_decision_val)
      
      
      # not dealing with tied ranks which perhaps I should
      the_ranks <- rowSums(diff_decision_vals < 0)
      normalized_rank_results <- the_ranks/(dim(data_matrix)[2] - 1)
      
      
      summarized_normalized_rank_results <- prediction_results %>%
        select(-starts_with("decision")) %>%
        mutate(normalized_rank = normalized_rank_results) %>%
        dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
        summarize(normalized_rank = mean(.data$normalized_rank, na.rm = TRUE))
      
      
      the_results <- left_join(the_results, summarized_normalized_rank_results,
                               by = c("CV", "train_time", "test_time"))
      

      # clean up memory
      rm(data_matrix)
      rm(diff_decision_vals)
      gc()
    
  }  # end for decision values and normalized rank results
  
  

  options <- attr(rm_obj, "options")
  options$zero_one_loss_chance_level <- zero_one_loss_chance_level 
  
  new_rm_main_results(
    the_results,
    "results combined over one cross-validation split",
    options)
  

}
  
  






# The aggregate_resample_run_results method needed to fulfill the results metric interface
aggregate_resample_run_results.rm_main_results <- function(resample_run_results) {
  
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


  new_rm_main_results(
    central_results,
    "final results",
    attributes(resample_run_results)$options)
  
}





#' A plot function for the rm_main_results object
#'
#' This function can create a line plot of the results or temporal
#' cross-decoding results for the the zero-one loss, normalized rank and/or
#' decision values after the decoding analysis has been run (and all results
#' have been aggregated).
#'
#' @param x A rm_main_result object that has aggregated runs from a
#'   decoding analysis, e.g., if DECODING_RESULTS are the out from the
#'   run_decoding(cv) then this argument should be
#'   `DECODING_RESULTS$rm_main_results`.
#'
#' @param ... This is needed to conform to the plot generic interface.
#'
#' @param results_to_show A string specifying the types of results to plot. Options
#'   are: 'zero_one_loss', 'normalized_rank', 'decision_values', or 'all'.
#'
#' @param type A string specifying the type of results to plot. Options are
#'   'TCD' to plot a temporal cross decoding matrix or 'line' to create a line
#'   plot of the decoding results as a function of time
#'
#' @family result_metrics
#'
#'
#'
#' @export
plot.rm_main_results <- function(x, ..., results_to_show = "zero_one_loss", type = "TCD") {
  
  main_results <- x

  if (attributes(main_results)$state != "final results") {
    stop("The results can only be plotted *after* the decoding analysis has been run")
  }


  # convert the zero-one loss results to percentages
  main_results <- dplyr::mutate(main_results, zero_one_loss = .data$zero_one_loss * 100)


  # parse which type of results should be plotted
  if (results_to_show == "all") {
    
    # do nothing
    
  } else if (results_to_show == "zero_one_loss") {
    
    main_results <- dplyr::select(main_results, .data$train_time, .data$test_time, .data$zero_one_loss)
    
  } else if (results_to_show == "normalized_rank") {
    
    if (!(results_to_show %in% names(main_results))) {
      stop(paste("Can't plot", results_to_show, "results because this type of result was not saved."))
    }

    main_results <- dplyr::select(main_results, .data$train_time, .data$test_time, .data$normalized_rank)
    
  } else if (results_to_show == "decision_vals") {
    
    if (!(results_to_show %in% names(main_results))) {
      stop(paste("Can't plot", results_to_show, "results because this type of result was not saved."))
    }

    main_results <- dplyr::select(main_results, .data$train_time, .data$test_time, .data$decision_vals)
    
  } else {
    
    warning(paste0(
      "results_to_show must be set to either 'all', 'zero_one_loss', 'normalized_rank', or 'decision_vals'.",
      "Using the default value of all"))
  }



  if (!(type == "TCD" || type == "line")) {
    warning("type must be set to 'TCD' or 'line'. Using the default value of 'TCD'")
  }



  main_results$train_time <- round(get_center_bin_time(main_results$train_time))
  main_results$test_time <- round(get_center_bin_time(main_results$test_time))

  # an alternative way to display the labels (not used)
  # main_results$train_time <- get_time_range_strings(main_results$train_time)
  # main_results$test_time <- get_time_range_strings(main_results$test_time)

  
  main_results <- main_results %>%
    tidyr::gather("results_to_show", "accuracy", -.data$train_time, -.data$test_time) %>%
    dplyr::mutate(
      results_to_show = replace(results_to_show, results_to_show == "zero_one_loss", "Zero-one loss"),
      results_to_show = replace(results_to_show, results_to_show == "normalized_rank", "Normalized rank"),
      results_to_show = replace(results_to_show, results_to_show == "decision_vals", "Decision values"))

  
  # data frame for plotting a horizontal line at chance decoding accuracy levels
  #  (chance level from the input x which is the rm_main_results object passed to plot)
  zero_one_loss_chance <- 100 * attributes(x)$options$zero_one_loss_chance_level
  chance_accuracy_df <- data.frame(
    results_to_show = c("Zero-one loss", "Normalized rank", "Decision values"),
    chance_level = c(zero_one_loss_chance, .5, NA)) %>%
    dplyr::filter(.data$results_to_show %in% unique(main_results$results_to_show))
  
  
  # if only a single time, just plot a bar for the decoding accuracy
  if (length(unique(main_results$train_time)) == 1) {
    
    main_results %>%
      ggplot(aes(.data$test_time, .data$accuracy)) +
      geom_col() +
      facet_wrap(~results_to_show, scales = "free") +
      xlab("Time") +
      ylab("Accuracy")
    
  } else if ((sum(main_results$train_time == main_results$test_time) == dim(main_results)[1]) ||
    type == "line") {

    # if only trained and tested at the same time, create line plot
    main_results %>%
      dplyr::filter(.data$train_time == .data$test_time) %>%
      ggplot(aes(.data$test_time, .data$accuracy)) +
      facet_wrap(~results_to_show, scales = "free") +
      xlab("Time") +
      ylab("Accuracy") +
      geom_hline(data = chance_accuracy_df, 
                 aes(yintercept = .data$chance_level),
                 color = "maroon", na.rm=TRUE) + 
      geom_line() + 
      theme_classic() + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "white", fill = "white"))
      
    
  } else {

    # if trained and testing at all times, create a TCD plot

    if (results_to_show != "all") {
      
      g <- main_results %>%
        ggplot(aes(.data$test_time, .data$train_time, fill = .data$accuracy)) +
        geom_tile() +
        facet_wrap(~results_to_show) +
        scale_fill_continuous(type = "viridis", name = "Prediction \n accuracy") +
        ylab("Train time") +
        xlab("Test time") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(colour = "white", fill = "white"))

      g
      
    } else if (results_to_show == "all") {

      # plotting multiple TCD subplots on the same figure

      all_TCD_plots <- lapply(unique(main_results$results_to_show), function(curr_result_name) {
        curr_results <- filter(main_results, .data$results_to_show == curr_result_name)

        g <- curr_results %>%
          ggplot(aes(.data$test_time, .data$train_time, fill = .data$accuracy)) +
          geom_tile() +
          facet_wrap(~results_to_show, scales = "free") +
          # scale_fill_continuous(type = "viridis", name = curr_results$results_to_show[1]) +
          scale_fill_continuous(type = "viridis", name = "") +
          ylab("Train time") +
          xlab("Test time") +
          theme(legend.position = "bottom") +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(colour = "white", fill = "white"))
        
        g
        
      })

      all_TCD_plots[["ncol"]] <- 3
      do.call(gridExtra::grid.arrange, all_TCD_plots)
    }
    
  }
}




# Get the parameters for the rm_main_results object
#' @export
get_parameters.rm_main_results <- function(ndr_obj) {

  # get the options for now the normalized rank and decision values should be aggregated
  include_norm_rank_results <- attr(ndr_obj, "options")$include_norm_rank_results

  data.frame(rm_main_results.include_norm_rank_results = include_norm_rank_results)
  
}



