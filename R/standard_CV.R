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
    result_metrics <- list(normalized_rank_and_decision_values_PM(), 
                                confusion_matrix_PM())
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
  #ALL_DECODING_RESULTS <- foreach(iResample = 1:num_resample_runs, 
  #                                .export=c('get_rank_results')) %dopar% {  # %dopar% {  
  
  ALL_DECODING_RESULTS <- foreach(iResample = 1:num_resample_runs) %dopar% {  # %dopar% {  
                                                                      
                                    
                                    
    # get the data from the current cross-validation run
    cv_data <- get_data(datasource)  
    

    unique_times <- unique(cv_data$time_bin)
    num_time_bins <- length(unique_times)
    all_cv_train_test_inds <- select(cv_data, starts_with("CV"))
    num_CV <- ncol(all_cv_train_test_inds)
  
    # add names for the different dimensions of the results
    time_names <- grep("^time", names(datasource$binned_data), value = TRUE)
    #dim_names <- list(1:num_CV, time_names, time_names)
  
    
    DECODING_RESULTS <- NULL
    
    
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
      DECODING_RESULTS[[iMetric]] <- curr_metric_results
    }
    
    
    return(DECODING_RESULTS)
    
    
    
  }  # end loop over resample runs



  
  # aggregate results over all resample runs  ---------------------------------

  # close parallel resources
  doParallel::stopImplicitCluster()
  
  

  grouped_results <- purrr::transpose(ALL_DECODING_RESULTS)

  
  
  # should have loop that goes through all PMs and gets their final results...
  
  resample_run_results <- dplyr::bind_rows(grouped_results[[2]], .id = "resample_run")

  confusion_matrix <- aggregate_resample_run_results(resample_run_results)
  
  
  browser()
  
  
  plot(confusion_matrix)
  
  
  other_results <- dplyr::bind_rows(grouped_results[[1]], .id = "resample_run")
  
  
  #####
  
  
  
  
  
  
  # Have a for loop here where each PM object's aggregrate_resample_run functions are called...
  #group_results[[iMetric]]
  
  
  # what to do about MI which is calculated on the confusion matrices? 
  
  # Also need to add:
  #  1) normalized rank confusion matrices (computed from rank results)
  #  2) ROC AUC metric  (why not)
  #  3) Other things 
  #      - Plot functions for these S3 objects
  #      - Saving parameters from the ds, cl, cv objects (or should this be done at a higher level?)
  
  
  
  
  ### everything below is basically junk
  
  mean_decoding_results <- purrr::map(ALL_DECODING_RESULTS, 'mean_decoding_results') 
  
  
 
  blah <- unlist(ALL_DECODING_RESULTS, recursive = FALSE)

  
  # works when there is only one metric...
  #blarg <- dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  
  
  # ####
  # 
  # # Experimenting with code if I don't collapse across CV runs but save all results until the end...
  # # This will almost certainly have too large of a memory footprint so can't do this...
  # 
  # all_decoding_results <- dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  # 
  # confusion_matrix <- get_confusion_matrix(all_decoding_results)
  # 
  # 
  # # add in the normalized rank and decision value results
  # results <- cbind(all_decoding_results, get_rank_results(all_decoding_results))
  # 
  # mean_decoding_results <- results %>%
  #    group_by(train_time, test_time, CV, resample_run) %>%
  #    summarize(zero_one_loss = mean(correct),
  #              normalized_rank = mean(normalized_rank_results),
  #              decision_vals = mean(correct_class_decision_val))
  # 
  # 
  # ####
  
  
  browser()
  
  mean_decoding_results <- purrr::map(ALL_DECODING_RESULTS, 'mean_decoding_results') 
  all_mean_decoding_results <- dplyr::bind_rows(mean_decoding_results, .id = "resample_run")

  
  
  
  
  # return all the decoding results collapsed into one data frame
  dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  

}  # end the run_decoding method




