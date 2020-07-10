#' The standard cross-validator (CV) object
#'
#' This uses cross-validation to run a decoding analysis
#'
#' @details A cross-validator object takes a datasource (DS), a classifier (CL),
#' feature preprocessors (FP) and result metric (RM) objects, and runs multiple
#' cross-validation cycles by getting new training and test data splits, running
#' the preprocessor to do preprocessing of the data, trains and tests the
#' classifier, and uses the result metric objects to evaluate the classification
#' performance on the test set.
#'
#' @param datasource a datasource (DS) object that will generate the training
#'   and test data
#'
#' @param classifier a classifier (CS) object that will learn parameters based
#'   on the training data and will generate predictions based on the test data.
#'
#' @param feature_preprocessors a list of feature preprocessor (FP) objects that
#'   learn preprocessing parameters from the training data and apply
#'   preprocessing of both the training and test data based on these parameters
#'
#' @param result_metrics a list of result metric (RM) objects that are used to
#'   evaluate the classification performance. If this is set to null then the 
#'   rm_main_results(), rm_confusion_matrix() results metrics will be used. 
#'   
#' @param num_resample_runs The number of times the cross-validation should be
#'   run (i.e., "resample runs"), where on each run, new training and test sets
#'   are generated. If pseudo-populations are used (say with the ds_basic) then
#'   new pseduo-populations will be generated on each resample run as well.
#'
#' @param test_only_at_training_time Whether the analysis should only be run
#'   where the classifier is trained and tested at the same time point (i.e.,
#'   now temporal cross-decoding analysis will be run). Setting this to true can
#'   potentially speed up the analysis and save memory at the cost of not
#'   calculated the temporal cross-decoding results.
#'   
#' @param run_parallel A boolean to indicate whether the code should be run in
#'    parallel. It is useful to set this to FALSE for debugging purposes or 
#'    if you are running a job that takes a lot of memory and runtime is not
#'    of much concern.
#'
#' @examples
#' data_file <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NDTr") 
#' ds <- ds_basic(data_file, 'stimulus_ID', 18)
#' fps <- list(fp_zscore())
#' cl <- cl_max_correlation()
#' cv <- cv_standard(ds, cl, fps) 
#'
#'
#' @family cross-validator




# the constructor 
#' @export
cv_standard <- function(datasource, 
                        classifier, 
                        feature_preprocessors, 
                        result_metrics = NULL,
                        num_resample_runs = 50, 
                        test_only_at_training_time = FALSE,
                        run_parallel = TRUE) {
  
  if (is.null(result_metrics)) {
    result_metrics <- list(rm_main_results(), 
                                rm_confusion_matrix())
  }
        
  
  analysis_ID <- generate_analysis_ID()
  
  the_cv <- list(analysis_ID = analysis_ID, 
                 datasource = datasource, 
                 classifier = classifier,
                 feature_preprocessors = feature_preprocessors,
                 num_resample_runs = num_resample_runs,
                 result_metrics = result_metrics,
                 test_only_at_training_time = test_only_at_training_time,
                 run_parallel = run_parallel)
      
  attr(the_cv, "class") <- "cv_standard"
  the_cv 

}
  



#' @export
run_decoding.cv_standard = function(cv_obj) {
  
  
  analysis_start_time <- Sys.time()
  
  # copy over the main objects
  datasource <- cv_obj$datasource
  classifier = cv_obj$classifier
  feature_preprocessors = cv_obj$feature_preprocessors
  num_resample_runs = cv_obj$num_resample_runs
  result_metrics = cv_obj$result_metrics
  test_only_at_training_time = cv_obj$test_only_at_training_time
  run_parallel <- cv_obj$run_parallel  


  
  if (run_parallel) {
  
    # register parallel resources
    cores <- parallel::detectCores()
    #the_cluster <- parallel::makeCluster(cores)
    #doParallel::registerDoParallel(the_cluster)
  
    # switching to the doSNOW package b/c there seems to be a memory leak with doParallel
    the_cluster <- makeCluster(cores, type="SOCK")
    doSNOW::registerDoSNOW(the_cluster)
  
    '%do_type%' <- get('%dopar%') 
    
  } else {
    
    '%do_type%' <- get('%do%') 
  }
  
  
  
  # Do a parallel loop over resample runs
  all_resample_run_decoding_results <- foreach(iResample = 1:num_resample_runs) %do_type% {  # %dopar% {  
    
    
    # get the data from the current cross-validation run
    cv_data <- get_data(datasource)  
    
    unique_times <- unique(cv_data$time_bin)
    num_time_bins <- length(unique_times)
    all_cv_train_test_inds <- select(cv_data, starts_with("CV"))
    num_CV <- ncol(all_cv_train_test_inds)
  

    # resample_run_decoding_results is the name of the decoding results inside the dopar loop
    # outside the loop, when all the results have really been combined into a list, 
    # this is called all_resample_run_decoding_results
    resample_run_decoding_results <- NULL   
    
    all_cv_results <- NULL
    
    for (iCV in 1:num_CV) {

      all_time_results <- NULL
      
      tictoc::tic()
      print(paste0("CV: ", iCV))
      
      for (iTrain in 1:num_time_bins) {
        
        training_set <- dplyr::filter(cv_data, .data$time_bin == unique_times[iTrain], all_cv_train_test_inds[iCV] == "train") %>% 
          dplyr::select(starts_with("site"), .data$train_labels)
        
        test_set <- dplyr::filter(cv_data, all_cv_train_test_inds[iCV] == "test") %>% 
          dplyr::select(starts_with("site"), .data$test_labels, .data$time_bin) 

        if (test_only_at_training_time) {
          test_set <- dplyr::filter(test_set, .data$time_bin == unique_times[iTrain])
        }
     
        # if feature-processors have been specified, do feature processing...
        if (length(feature_preprocessors) >= 1) {
          for (iFP in 1:length(feature_preprocessors)) {
            
            processed_data <- preprocess_data(feature_preprocessors[[iFP]], training_set, test_set)
            training_set <- processed_data$training_set 
            test_set <- processed_data$test_set 
            
          }
        }  # end the if statement for doing preprocessing
  

        
        # get predictions from the classifier (along with the correct labels)
        curr_cv_prediction_results <- get_predictions(classifier, training_set, test_set)

        # add the current CV run number, train time to the results data frame
        curr_cv_prediction_results <- curr_cv_prediction_results %>%
          dplyr::mutate(CV = iCV, train_time = unique_times[iTrain]) %>%
          select(.data$CV, .data$train_time, everything())
        
        
        #all_cv_results <- rbind(all_cv_results, curr_cv_prediction_results)
        all_time_results[[iTrain]] <- curr_cv_prediction_results   # should be faster b/c don't need to reallocate memory

        
        
      }   # end the for loop over time bins
      tictoc::toc()
  
      
      # Aggregate results over all CV split runs
      all_cv_results[[iCV]] <- dplyr::bind_rows(all_time_results)
      
      
    }  # end the for loop over CV splits
  
  

    # convert the results from each CV split from a list into a data frame
    all_cv_results <- dplyr::bind_rows(all_cv_results)
    
    
    
    # go through each Result Metric and aggregate the results from all CV splits using each metric
    for (iMetric in 1:length(result_metrics)) {
      curr_metric_results <- aggregate_CV_split_results(result_metrics[[iMetric]], all_cv_results)
      resample_run_decoding_results[[iMetric]] <- curr_metric_results   ###  DECODING_RESULTS
    }

        
    
    # save decoding parameters...


    return(resample_run_decoding_results) 
    
    
    
  }  # end loop over resample runs



  
  # aggregate results over all resample runs  ---------------------------------

  # close parallel resources
  #doParallel::stopImplicitCluster()
  
  # switching to the doSNOW package b/c there seems to be a memory leak with doParallel
  if (run_parallel) {
    stopCluster(the_cluster)  
  }
  
  
  # go through each Result Metric and aggregate the final results from all resample runs using each metric
  DECODING_RESULTS <- NULL
  result_metric_names <- NULL
  grouped_results <- purrr::transpose(all_resample_run_decoding_results)
  for (iMetric in 1:length(result_metrics)) {
    
    # bind the list of all the resample result RM objects together and preserve the RM's options attribute
    curr_options = attributes(grouped_results[[iMetric]][[1]])$options 
    curr_resample_run_results <- dplyr::bind_rows(grouped_results[[iMetric]], .id = "resample_run")
    attr(curr_resample_run_results, "options") <- curr_options
    
    DECODING_RESULTS[[iMetric]] <- aggregate_resample_run_results(curr_resample_run_results)
    result_metric_names[iMetric] <- class(DECODING_RESULTS[[iMetric]])[1]
  
  }
  
  
  # add names to the final results list so easy to extract elements
  names(DECODING_RESULTS) <- result_metric_names
    
  
  
  # save the decoding parameters to make results reproducible -----------------
  
  # set to null to save memory, can recreate the datasource by reloading the 
  #  data in the binned_file_name field
  cv_obj$datasource$binned_data <- NULL
  
  
  cv_obj$parameter_df <- get_parameters(cv_obj)
  
  analysis_end_time <- Sys.time()
  
  # could save these in the cv_obj directly rather than in the cv_obj$parameters_df
  cv_obj$parameter_df$analysis_start_time <- analysis_start_time
  cv_obj$parameter_df$analysis_end_time <- analysis_end_time
  
  # saves all the CV parameters (datasource, classifier feature preprocessros etc)
  DECODING_RESULTS$cross_validation_paramaters <- cv_obj

  
  return(DECODING_RESULTS)
  
  

}  # end the run_decoding method




get_parameters.cv_standard = function(ndtr_obj){
  
  # get parameters from all objects and save the in a data frame so that
  # which will be useful to tell if an analysis has already been run
  
  # start by getting the parameters from the datasource
  parameter_df <- get_parameters(ndtr_obj$datasource)
  
  
  # add the parameters from the classifier
  parameter_df <- cbind(parameter_df, get_parameters(ndtr_obj$classifier))
  
  
  # if feature-processors have been specified, add their parameters to the data frame
  if (length(ndtr_obj$feature_preprocessors) >= 1) {
    for (iFP in 1:length(ndtr_obj$feature_preprocessors)) {
      curr_FP_parameters <- get_parameters(ndtr_obj$feature_preprocessors[[iFP]])
      parameter_df <- cbind(parameter_df, curr_FP_parameters)
    }
  }  # end the if statement for doing preprocessing
  
  
  
  # go through each Result Metric and get their parameters
  for (iMetric in 1:length(ndtr_obj$result_metrics)) {
    curr_metric_parameters <- get_parameters(ndtr_obj$result_metrics[[iMetric]])
    parameter_df <- cbind(parameter_df, curr_metric_parameters)
  }
  
  
  
  # finally add the parameters from this cv_standard object as well

  cv_parameters <- data.frame(analysis_ID = ndtr_obj$analysis_ID, 
                              cv_standard.num_resample_runs = ndtr_obj$num_resample_runs, 
                              cv_standard.test_only_at_training_time = ndtr_obj$test_only_at_training_time)
  
  
  parameter_df <- cbind(cv_parameters, parameter_df)
  
  parameter_df
  
  
}
