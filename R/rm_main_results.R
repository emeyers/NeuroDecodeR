#' A result metric (RM) that calculates main decoding accuracy measures
#'
#' This result metric calculate the zero-one loss, the normalized rank, and the
#' mean of the decision values. This is also an S3 object which has an
#' associated plot function to display the results.
#'
#' @details
#' Like all result metrics, this result metric has functions to aggregate
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
#'   string set to "only_same_train_test_time", then the normalized rank and
#'   decision values will only be calculated when for results when training and
#'   testing at the same time. Not returning the full results can speed up the
#'   run-time of the code and will use less memory so this can be useful for
#'   large data sets.
#'
#' @return This constructor creates an NDR result metric object with the class
#'   `rm_main_results`. Like all NDR result metric objects, this result
#'   metric will be used by a cross-validator to create a measure of decoding
#'   accuracy by aggregating the results after all cross-validation splits have
#'   been run, and after all resample runs have completed.
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
#' @inherit aggregate_CV_split_results
#' @keywords internal
#' @export
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

  
if ((sum(grepl("decision", names(prediction_results))) == 0) & (include_norm_rank_results != FALSE)) {
  
  # Perhaps this should be an error instead of just resetting the argument value which in general
  #  is a pretty bad thing to do. However, likely 
  error_message <- paste("The classifier selected did not returned decision values.", 
                         "Setting argument 'include_norm_rank_results' to FALSE.\n")
  
  warning(error_message)
  
  # change the parameter value to the warning message to let the user know this
  # parameter was reset when they look at the cross-validation parameters
  attr(rm_obj, "options")$include_norm_rank_results <- error_message
  
  include_norm_rank_results <- FALSE
  
}
 
 
  # get the zero-one loss loss results
  the_results <- prediction_results %>%
    dplyr::mutate(correct = .data$actual_labels == .data$predicted_labels) %>%
    #dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
    dplyr::group_by(.data$train_time, .data$test_time) %>%           # also aggregating over CV splits here
    summarize(zero_one_loss = mean(.data$correct, na.rm = TRUE),
              sd_zero_one_loss = sd(.data$correct, na.rm = TRUE),
              se_zero_one_loss = sd(.data$correct, na.rm = TRUE)/sqrt(length(na.omit(.data$correct))))

  
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
      # dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%    
      dplyr::group_by(.data$train_time, .data$test_time) %>%     # also aggregating over CV splits here
      summarize(sd_decision_vals = sd(.data$decision_vals, na.rm = TRUE),
                se_decision_vals = sd(.data$decision_vals, na.rm = TRUE)/sqrt(length(na.omit(.data$decision_vals))),
                decision_vals = mean(.data$decision_vals, na.rm = TRUE)) %>%
      select("decision_vals", "sd_decision_vals", "se_decision_vals", everything())
    
      
      the_results <- left_join(the_results, summarized_correct_decision_val_results,
                               by = c("train_time", "test_time"))


    # get the normalized rank results...
      diff_decision_vals <- sweep(data_matrix, 1, correct_class_decision_val)
      
      
      # not dealing with tied ranks which perhaps I should
      the_ranks <- rowSums(diff_decision_vals < 0)
      normalized_rank_results <- the_ranks/(dim(data_matrix)[2] - 1)
      
      
      summarized_normalized_rank_results <- prediction_results %>%
        select(-starts_with("decision")) %>%
        mutate(normalized_rank = normalized_rank_results) %>%
        # dplyr::group_by(.data$CV, .data$train_time, .data$test_time) %>%
        dplyr::group_by(.data$train_time, .data$test_time) %>%    # also aggregating over CV splits here
        summarize(sd_normalized_rank = sd(.data$normalized_rank, na.rm = TRUE),
                  se_normalized_rank = sd(.data$normalized_rank, na.rm = TRUE)/sqrt(length(na.omit(.data$normalized_rank))),
                  normalized_rank = mean(.data$normalized_rank, na.rm = TRUE)) %>%
        select("normalized_rank", "sd_normalized_rank", "se_normalized_rank", everything())
      
      the_results <- left_join(the_results, summarized_normalized_rank_results,
                               by = c("train_time", "test_time"))
      

      # clean up memory
      rm(data_matrix)
      rm(diff_decision_vals)
      gc()
    
  }  # end for decision values and normalized rank results
  
  
  # try to clear up even more memory (does this do anything?)
  rm(prediction_results)
  
  options <- attr(rm_obj, "options")
  options$zero_one_loss_chance_level <- zero_one_loss_chance_level 
  
  new_rm_main_results(
    the_results,
    "results combined over one cross-validation split",
    options)
  
}
  
  






# The aggregate_resample_run_results method needed to fulfill the results metric interface
#' @inherit aggregate_resample_run_results
#' @keywords internal
#' @export
aggregate_resample_run_results.rm_main_results <- function(resample_run_results) {
  
  
  central_results <- resample_run_results %>%
    group_by(.data$train_time, .data$test_time) %>%
    summarize(zero_one_loss = mean(.data$zero_one_loss),
              sd_zero_one_loss = mean(.data$sd_zero_one_loss),
              se_zero_one_loss = mean(.data$se_zero_one_loss))


  if ("normalized_rank" %in% names(resample_run_results)) {
    
    normalized_rank_results <- resample_run_results %>%
      group_by(.data$train_time, .data$test_time) %>%
      summarize(normalized_rank = mean(.data$normalized_rank),
                sd_normalized_rank = mean(.data$sd_normalized_rank),
                se_normalized_rank = mean(.data$se_normalized_rank))

    central_results <- left_join(central_results, normalized_rank_results, by = c("train_time", "test_time"))
  }


  if ("decision_vals" %in% names(resample_run_results)) {
    
    decision_vals_results <- resample_run_results %>%
      group_by(.data$train_time, .data$test_time) %>%
      summarize(decision_vals = mean(.data$decision_vals),
                sd_decision_vals = mean(.data$sd_decision_vals),
                se_decision_vals = mean(.data$se_decision_vals))

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
#'   plot of the decoding results as a function of time.
#'   
#' @param errorbar A string specifying if error bars should be plotted. Options
#'  are: 'sd', 'se', or '2se'. If this is set to NULL, then no error bars will
#'  be plotted. If this is set to 'sd', then the standard deviation of the
#'  results will be plotted. If this is set to 'se', then the standard error of
#'  the results will be plotted. If this is set to '2se', then two times the
#'  standard error of the results will be plotted (which is often used to
#'  represent a 95% confidence interval). Note, these error bars are slight 
#'  underestimates of the sd and sderr because when using cross-validation the 
#'  test data is not independent of the training data. Also, note that error 
#'  bars can only be plotted for line plots and not for TCD plots. 
#'  
#' @return Returns a ggplot object that plots the main results.
#'
#' @family result_metrics
#'
#'
#'
#' @export
plot.rm_main_results <- function(x, ..., results_to_show = "zero_one_loss", errorbar = NULL, type = "TCD") {

  # call the helper function to do all the hard work
  helper_plot_rm_main_results(x, results_to_show, type, errorbar)

}
  
 


# Get the parameters for the rm_main_results object
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.rm_main_results <- function(ndr_obj) {

  # get the options for now the normalized rank and decision values should be aggregated
  include_norm_rank_results <- attr(ndr_obj, "options")$include_norm_rank_results

  data.frame(rm_main_results.include_norm_rank_results = include_norm_rank_results)
  
}








#' A plot function to plot multiple rm_main_results 
#'
#' This function can create a line plot of the results or temporal
#' cross-decoding results for the the zero-one loss, normalized rank and/or
#' decision values after the decoding analysis has been run (and all results
#' have been aggregated).
#'
#' @param results_dir_name A string specifying the directory name that contains
#'  files with DECODING_RESULTS that have rm_main_results as one of the result
#'  metrics. 
#'
#' @param results_to_plot This can be set to a vector of strings specifying
#'   result_names for the results to plot, or a vector of numbers that contain
#'   the rows in the results_manifest file of the results that should be
#'   compared. The results_manifest file should be created from saving results
#'   using the log_save_results() function. Finally, if this is set to a single
#'   string that is a regular expression, all results in the results_manifest
#'   file result_name variable that match the regular expression will be
#'   plotted.
#'
#' @param results_to_show A string specifying the types of results to plot. Options
#'   are: 'zero_one_loss', 'normalized_rank', 'decision_values', or 'all'.
#'
#' @param type A string specifying the type of results to plot. Options are
#'   'TCD' to plot a temporal cross decoding matrix or 'line' to create a line
#'   plot of the decoding results as a function of time.
#'   
#' @param errorbar A string specifying if error bars should be plotted. Options
#'  are: 'sd', 'se', or '2se'. If this is set to NULL, then no error bars will
#'  be plotted. If this is set to 'sd', then the standard deviation of the
#'  results will be plotted. If this is set to 'se', then the standard error of
#'  the results will be plotted. If this is set to '2se', then two times the
#'  standard error of the results will be plotted (which is often used to
#'  represent a 95% confidence interval). Note, these error bars are slight 
#'  underestimates of the sd and sderr because when using cross-validation the 
#'  test data is not independent of the training data. Also, note that error 
#'  bars can only be plotted for line plots and not for TCD plots. 
#'   
#' @param display_names A vector of strings specifying what the labels on the
#'   plots should say for each result. If this is NULL, the result names will be
#'   the names from the manifest file's result_name column, or if these are set
#'   to "No result name set" then the analysisID will be the label. 
#'   
#' @return Returns a ggplot object that a comparison of main decoding results.
#'   
#' @family result_metrics
#'
#' @export
plot_main_results <- function(results_dir_name, 
                              results_to_plot, 
                              results_to_show = "zero_one_loss", 
                              type = "line",
                              errorbar = NULL,
                              display_names = NULL) {
  
  
  # adding these so that the code passes R CMD checks
  DECODING_RESULTS <- NULL
  manifest_df <- NULL

  
  if(!("results_manifest.rda" %in% list.files(results_dir_name))) {
      stop("A manifest file does not exist in the results_dir_name name specified.")
  }

  
  if ( (!is.null(display_names)) && (length(display_names) != length(results_to_plot)) ) {
    stop("The display_names argument must be set to NULL, a vector of strings the same length as the 
              number of results to be plotted or to a regular expression.")
  }

  
  
  # A helper function that extracts the main results
  extract_main_results <- function(decoding_results, result_name) {
    
    # check that rm_main_results exist in the current DECODING_RESULTS file
    if (!("rm_main_results" %in% names(decoding_results))) {
      
      # if decoding_results is a list containing multiple DECODING_RESULTS, 
      #  create a data frame with all the rm_main_results together

      # very ugly/hacky solution to get unique names but quick fix for now
      multi_result_names <- names(decoding_results)
      if (length(unique(multi_result_names)) != length(multi_result_names)) {
        multi_result_names <- paste(multi_result_names, "--", sprintf("%003d", 1:length(multi_result_names)))
      }
      
      for (iMultiResult in 1:length(decoding_results)) {
        
        curr_multi_result <- decoding_results[[iMultiResult]]
        
        # rm_main_results was not saved in one of the result files
        if (!("rm_main_results" %in% names(curr_multi_result))) { 
          stop(paste("The DECODING_RESULTS named", result_name, "with analysis_ID",  
                     decoding_results$cross_validation_paramaters$analysis_ID,
                     "does not contain rm_main_results so can not plot this result."))
        }
        
        curr_multi_result <- curr_multi_result$rm_main_results
        curr_multi_result$result_name <- multi_result_names[iMultiResult]
        
        if (iMultiResult == 1) {
          curr_rm_main_results <- curr_multi_result
        } else {
          curr_rm_main_results <- rbind(curr_rm_main_results, curr_multi_result)
        }
        
      }
      
      
    } else {
      
      curr_rm_main_results <- decoding_results$rm_main_results
      curr_rm_main_results$result_name <- result_name
    }
    

    curr_rm_main_results
    
  }
  
  
  
  all_main_results <- data.frame()
  for (iResult in seq_along(results_to_plot)) {
   
    if (is.character(results_to_plot)) {
      
      # load all results based on their result_names - this could be a list that has multiple DECODING_RESULTS
      DECODING_RESULTS <- log_load_results_from_result_name(results_to_plot[iResult], results_dir_name)
      
      # use the display_names if they are not null
      if (!is.null(display_names)) {
        curr_result_name <- display_names[iResult]
      } else {
        curr_result_name <- results_to_plot[iResult]
      }
      
    }  else if (is.numeric(results_to_plot)) {
      
      # if results_to_plot is a numeric vector, treat it as rows in the manifest file to use
      
      load(file.path(results_dir_name, "results_manifest.rda"))
      load(file.path(results_dir_name, paste0(manifest_df$analysis_ID[results_to_plot[iResult]], ".rda")))
        
      # use the display_names if they are not null
      if (!is.null(display_names)) {
        curr_result_name <- display_names[iResult]
      } else {
        curr_result_name <- manifest_df[results_to_plot[iResult], c("result_name")]
        if (curr_result_name == "No result name set") {
          curr_result_name <- manifest_df$analysis_ID[results_to_plot[iResult]]
        }
      }
        
    }
      
      
      curr_rm_main_results <- extract_main_results(DECODING_RESULTS, curr_result_name)
      
      all_main_results <- rbind(all_main_results, curr_rm_main_results)

  }
    
  
  # plot the results using the helper_plot_rm_main_results() helper function
  helper_plot_rm_main_results(all_main_results, results_to_show, type, errorbar)
  
  
}









# A private helper function that does all the hard work of plotting the results
helper_plot_rm_main_results <- function(main_results, 
                                        results_to_show = "zero_one_loss", 
                                        type = "TCD",
                                        errorbar = NULL) {  
  

  
  # sanity check that only trying to plot final aggregated results
  if (attributes(main_results)$state != "final results") {
    stop("The results can only be plotted *after* the decoding analysis has been run")
  }  
  
  
  # check arguments are valid
  if (!(type == "TCD" || type == "line")) {
    stop("type must be set to 'TCD' or 'line'. Using the default value of 'TCD'")
  }
  
  
  # check if errorbar has been supplied when trying to create a TCD plot
  if ((type == "TCD") && !is.null(errorbar)) {
     message("Can't plot errorbars for TCD plots. Ignoring the errorbar argument.")
  }
  
  
  valid_result_names <- c('all', 'zero_one_loss', 'normalized_rank', 'decision_vals')
  if (!(sum(results_to_show %in% valid_result_names) == length(results_to_show))) {
    stop(paste0(
      "results_to_show must be set to either 'all', 'zero_one_loss', 'normalized_rank', or 'decision_vals'.",
      "Using the default value of all"))
  }
  
  
  # expand to result names if "all" was passed as an argument
  if (results_to_show[1] == "all") {
    results_to_show <- c("zero_one_loss",  "normalized_rank", "decision_vals")
  }
 
  # normalized rank and decision values are not always saved so check that they exist if plotted them  
  if (!(sum(results_to_show %in% names(main_results)) == length(results_to_show))) {
    stop(paste("Can't plot", setdiff(results_to_show, names(main_results)), 
               "results because this type of result was not saved. \n  "))
  }
  
  # get the chance level for the zero one loss results
  zero_one_loss_chance <- 100 * attributes(main_results)$options$zero_one_loss_chance_level
  
  # convert the zero-one loss results to percentages
  main_results <- dplyr::mutate(main_results, zero_one_loss = .data$zero_one_loss * 100) |>
    dplyr::mutate(sd_zero_one_loss = .data$sd_zero_one_loss * 100) |>
    dplyr::mutate(se_zero_one_loss = .data$se_zero_one_loss * 100)
  
  main_results$train_time <- floor(get_time_bin_center(main_results$train_time))
  main_results$test_time <- floor(get_time_bin_center(main_results$test_time))
  
  # an alternative way to display the labels (not used)
  # main_results$train_time <- get_time_range_strings(main_results$train_time)
  # main_results$test_time <- get_time_range_strings(main_results$test_time)
  
  
  # remove result types that are not being used
  result_types_to_remove <- setdiff(c("zero_one_loss",  "normalized_rank", "decision_vals"), results_to_show)
  result_types_to_remove <- intersect(names(main_results), result_types_to_remove) # in case "normalized_rank", "decision_vals" are not in the results
  main_results <- dplyr::select(main_results, -contains({{result_types_to_remove}}))
  
  
  # keep only the errorbars needed
  if (is.null(errorbar)) {
      errorbar_df <- NULL
  } else if (errorbar == "sd") {
      errorbar_df <- select(main_results, contains("time"), contains("result_name"), contains("sd"))
  } else if (errorbar == "se") {
      errorbar_df <- select(main_results, contains("time"), contains("result_name"), contains("se"))
  } else if (errorbar == "2se") {
      errorbar_df <- cbind(select(main_results, contains("time")), select(main_results, contains("result_name")),
                           2 * select(main_results, contains("se")))
  } else {
      message("errorbar must be set to the string 'sd', 'se', or '2se'. Ignoring the errorbar argument.")
  }
      
  
  
  # remove the errorbars from the main results
  main_results <-  dplyr::select(main_results, -contains("sd"), -contains("se"))
  
  
  # pivot the main_results and errorbar_df to make then long and then join them
  main_results <- main_results %>%
    tidyr::pivot_longer(all_of(results_to_show), 
                        names_to = "results_to_show",
                        values_to = "accuracy") 

  
  
  # if plotting errorbars, add them to the results
  if (!is.null(errorbar)) {
    
    cols_not_to_pivot <- c("train_time", "test_time")
    
    if ("result_name" %in% names(errorbar_df)) {
      cols_not_to_pivot <- c(cols_not_to_pivot, "result_name")
    }
    

    errorbar_df <- errorbar_df %>%
      tidyr::pivot_longer(!all_of(cols_not_to_pivot),
                          names_to = "results_to_show",
                          values_to = "errorbar") %>%
      dplyr::mutate(results_to_show = substring(results_to_show, 4, nchar(results_to_show))) 
      
    
    main_results <- main_results |>
      left_join(errorbar_df, by = c(cols_not_to_pivot, "results_to_show"))
  }
  

  
  
  # rename results for better printing on plots and order according to the order given
  plot_result_names <- c("Zero-one loss", "Normalized rank", "Decision values")
  names(plot_result_names) <- c("zero_one_loss", "normalized_rank", "decision_vals")
  main_results$results_to_show <- factor(main_results$results_to_show, 
                                         levels = unique(main_results$results_to_show), 
                                         labels = plot_result_names[results_to_show])

  # create data frame for chance decoding levels
  chance_accuracy_df <- data.frame(
    results_to_show = c("zero_one_loss", "normalized_rank", "decision_vals"),
    chance_level = c(zero_one_loss_chance, .5, NA)) %>%
    dplyr::mutate(results_to_show = factor(.data$results_to_show,
                                           levels = results_to_show,
                                           labels = plot_result_names[results_to_show],
                                           ordered = TRUE)) %>%
    dplyr::filter(.data$results_to_show %in% unique(main_results$results_to_show))
  
  

  
  

  # actually plot the data...
  
  
  # if only a single time, just plot a bar for the decoding accuracy
  if (length(unique(main_results$train_time)) == 1) {
    
    # the "result_name" variable indicates there are multiple results 
    # so plot them in different colors...
    if ("result_name" %in% names(main_results)) {
      g <- main_results %>%
        ggplot(aes(.data$result_name, .data$accuracy, fill = .data$result_name)) 
      
    } else {
      g <- main_results %>%
        ggplot(aes(.data$test_time, .data$accuracy))
    }
    
    
    g <- g  +
      geom_col() + 
      facet_wrap(~results_to_show, scales = "free") +
      xlab("Time") +
      ylab("Accuracy")
    
    
    # should add an errorbar here too if applicable...
    
    g
    
    
  } else if ((sum(main_results$train_time == main_results$test_time) == dim(main_results)[1]) ||
             type == "line") {
    
    # if only trained and tested at the same time, create line plot
    
    
    # make sure only plot the results where the train and test times are the same
    main_results <- main_results |>
      dplyr::filter(.data$train_time == .data$test_time)
    
    
    # the "result_name" variable indicates there are multiple results to compare
    # so plot them in different colors...
    if ("result_name" %in% names(main_results)) {
      
      g <- main_results %>%
        ggplot(aes(.data$test_time, .data$accuracy, 
                   color = .data$result_name, fill = .data$result_name)) 
      
    } else {   # only a single result being plotted
      
      g <- main_results %>%
        ggplot(aes(.data$test_time, .data$accuracy)) 
    }
    
    
    g <- g +
      facet_wrap(~results_to_show, scales = "free") +
      xlab("Time") +
      ylab("Accuracy") +
      geom_hline(data = chance_accuracy_df, 
                 aes(yintercept = .data$chance_level),
                 color = "maroon", na.rm=TRUE) + 
      geom_line(linewidth = 1.2) + 
      theme_classic() + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "white", fill = "white"),
        legend.title=element_blank())

   
    
    # if there is an error bar, add it to the plot
    if (!is.null(errorbar)) {
      g <- g + geom_ribbon(data = main_results, 
                           aes(ymin = .data$accuracy - .data$errorbar, 
                               ymax = .data$accuracy + .data$errorbar,
                               color = NULL),
                           alpha = 0.2)
    }
    
    
    g   # return the ggplot object
    

    
  } else {
    
    # if trained and testing at all times, create a TCD plot
    
    if (length(results_to_show) == 1) {
      
      g <- main_results %>%
        ggplot(aes(.data$test_time, .data$train_time, fill = .data$accuracy)) +
        geom_tile() +
        scale_fill_continuous(type = "viridis", name = "Prediction \n accuracy") +
        ylab("Train time") +
        xlab("Test time") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(colour = "white", fill = "white"))
      
      
      # the "result_name" variable exists there are multiple results to compare
      # so facet on these different results
      if ("result_name" %in% names(main_results)) {
        
        g  <- g + facet_wrap(~result_name)
        
      } else {  # otherwise if only a single result, facet on the results_to_show 
        # that so the result_to_show is subtitle
        
        g  <- g + facet_wrap(~results_to_show)
        
      }
      
      
      g
      
      
      
    } else if (length(results_to_show) > 1)  {   # comparing multiple decoding result types with TCD plots
      
      
      # creating TCD plots of multiple result types comparing multiple results is not supported
      #  send a message saying this is not supported
      if ("result_name" %in% names(main_results)) { 
        
        stop(paste("Creating TCD plots comparing multiple results for multiple measures of decoding accuracy is not supported.",
                   "Please set the 'results_to_show' argument to a single string to plot only one result type;", 
                   "e.g., set results_to_show = 'zero_one_loss'."))
      }
      
      
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
      
      all_TCD_plots[["ncol"]] <- length(results_to_show)
      do.call(gridExtra::grid.arrange, all_TCD_plots)
    }
    
  }
}


