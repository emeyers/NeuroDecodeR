#' The standard cross-validator (CV)  object
#'
#' A cross-validator object takes a datasource, feature preprocessors and a classifier
#' and runs multiple cross-validation cycles by getting new training and test data splits, 
#' running the preprocessor to do preprocessing of the data, trains and tests the classifier, and 
#' the creates metric to evaluation the classification performance on the test set.  
#'
#'
#' @import parallel doParallel foreach




# the constructor 
#' @export
standard_CV <- function(datasource, 
                        classifier, 
                        feature_preprocessors, 
                        num_resample_runs = 50, 
                        result_metrics = NULL) {
  
  if (is.null(result_metrics)) {
    result_metrics <- list(main_results_RM(), 
                                confusion_matrix_RM())
  }
        
  
  the_cv <- list(datasource = datasource, 
                 classifier = classifier,
                 feature_preprocessors = feature_preprocessors,
                 num_resample_runs = num_resample_runs,
                 result_metrics = result_metrics)
      
  attr(the_cv, "class") <- "standard_CV"
  the_cv 

}
  



#run_decoding_one_resample_run <- function(cv_obj) {
#' @export
run_decoding.standard_CV = function(cv_obj) {
  

  # register parallel resources
  cores <- parallel::detectCores()
  the_cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(the_cluster)
  
  
  # copy over the main objects
  datasource <- cv_obj$datasource
  classifier = cv_obj$classifier
  feature_preprocessors = cv_obj$feature_preprocessors
  num_resample_runs = cv_obj$num_resample_runs
  result_metrics = cv_obj$result_metrics
  
  
  
  # Do a parallel loop over resample runs
  all_resample_run_decoding_results <- foreach(iResample = 1:num_resample_runs) %do% {  # %dopar% {  
                                                                      
                                    
    # get the data from the current cross-validation run
    cv_data <- get_data(datasource)  
    

    unique_times <- unique(cv_data$time_bin)
    num_time_bins <- length(unique_times)
    all_cv_train_test_inds <- select(cv_data, starts_with("CV"))
    num_CV <- ncol(all_cv_train_test_inds)
  
    # add names for the different dimensions of the results
    time_names <- grep("^time", names(datasource$binned_data), value = TRUE)
    #dim_names <- list(1:num_CV, time_names, time_names)
  
    
    # resample_run_decoding_results is the name of the decoding results inside the dopar loop
    # outside the loop, when all the results have really been combined into a list, 
    # this is called all_resample_run_decoding_results
    resample_run_decoding_results <- NULL   
    
    all_cv_results <- NULL
    
    for (iCV in 1:num_CV) {

      all_time_results <- NULL
      
      tictoc::tic()
      print(iCV)
      
      for (iTrain in 1:num_time_bins) {
        
        training_set <- dplyr::filter(cv_data, time_bin == unique_times[iTrain], all_cv_train_test_inds[iCV] == "train") %>% 
          dplyr::select(starts_with("site"), labels)
        
        test_set <- filter(cv_data, all_cv_train_test_inds[iCV] == "test") %>% 
          select(starts_with("site"), labels, time_bin) 
        
        
        # if feature-processors have been specified, do feature processing...
        if (length(feature_preprocessors) > 1) {
          for (iFP in 1:length(feature_preprocessors)) {
            
            processed_data <- preprocess_data(fps[[iFP]], training_set, test_set)
            training_set <- processed_data$training_set 
            test_set <- processed_data$test_set 
            
          }
        }  # end the if statement for doing preprocessing
  

        
        # get predictions from the classifier (along with the correct labels)
        curr_cv_prediction_results <- get_predictions(classifier, training_set, test_set)
        

        # add the current CV run number, train time to the results data frame
        curr_cv_prediction_results <- curr_cv_prediction_results %>%
          dplyr::mutate(CV = iCV, train_time = time_names[iTrain]) %>%
          select(CV, train_time, everything())
        
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
    
    
    return(resample_run_decoding_results) 
    
    
    
  }  # end loop over resample runs



  
  # aggregate results over all resample runs  ---------------------------------

  # close parallel resources
  doParallel::stopImplicitCluster()
  
  
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
    
  
  # add datasource parameters and cross-validation parameters to the results that are returned
  #...
  
  
  
  return(DECODING_RESULTS)
  
  
  # plot(DECODING_RESULTS$main_results_RM)
  # plot(DECODING_RESULTS$confusion_matrix_RM)
  
  
  # Also need to add:
  #  1) ROC AUC metric
  #  2) MI from confusion matrices, normalized rank confusion matrix
  #  3) Other things 
  #      - Plot functions for these S3 objects
  #      - Saving parameters from the ds, cl, cv objects (or should this be done at a higher level?)
  
  
  
  

}  # end the run_decoding method




