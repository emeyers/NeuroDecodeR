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
                        num_resample_runs = 50) {
        
  the_cv <- list(datasource = datasource, 
                 classifier = classifier,
                 feature_preprocessors = feature_preprocessors,
                 num_resample_runs = num_resample_runs)
      
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
  DECODING_RESULTS <- NULL
  
  
  # Do a parallel loop over resample runs
  ALL_DECODING_RESULTS <- foreach(iResample = 1:num_resample_runs, 
                                  .export=c('get_rank_results')) %dopar% {  # %dopar% {  
      
  # Non-parallel version - useful for debugging
  #ALL_DECODING_RESULTS <- foreach(iResample = 1:num_resample_runs, 
  #                                .export=c('get_rank_results')) %do% {
                                                                      
    # get the data from the current cross-validation run
    cv_data <- get_data(datasource)  
    

    unique_times <- unique(cv_data$time_bin)
    num_time_bins <- length(unique_times)
    all_cv_train_test_inds <- select(cv_data, starts_with("CV"))
    num_CV <- ncol(all_cv_train_test_inds)
  
    # add names for the different dimensions of the results
    time_names <- grep("^time", names(datasource$binned_data), value = TRUE)
    #dim_names <- list(1:num_CV, time_names, time_names)
  
    all_cv_results <- NULL
    
    for (iCV in 1:num_CV) {
  
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
        
        
        # moving this outside the CV loop
        
        # # add more measures of decoding accuracy (rank results, etc)
        # rank_and_decision_val_results <- get_rank_results(curr_cv_prediction_results)
        # results <- cbind(curr_cv_prediction_results, rank_and_decision_val_results)
        # 
        # 
        # 
        # # average the results over all predictions in this CV run (for each time bin)
        # mean_decoding_results <- results %>% 
        #   group_by(test_time) %>%
        #   summarize(zero_one_loss = mean(correct),
        #             normalized_rank = mean(normalized_rank_results),
        #             decision_vals = mean(correct_class_decision_val))
        # 
        # 
        # # add the current CV run number, train time to the results data frame
        # curr_results <- data.frame(CV = iCV, 
        #                            train_time = time_names[iTrain],
        #                            mean_decoding_results) 
        

        # add the current CV run number, train time to the results data frame
        curr_cv_prediction_results <- curr_cv_prediction_results %>%
          dplyr::mutate(CV = iCV, train_time = time_names[iTrain]) %>%
          select(CV, train_time, everything())
        
        #all_cv_results <- rbind(all_cv_results, curr_cv_prediction_results)
        all_cv_results[[iTrain]] <- curr_cv_prediction_results   # should be faster b/c don't need to reallocate memory
        
        
        #DECODING_RESULTS <- rbind(DECODING_RESULTS, curr_results)
        
        
      }   # end the for loop over time bins
      tictoc::toc()
  
    }  # end the for loop over CV splits
  
  
    # close parallel resources
    doParallel::stopImplicitCluster()
    
    # return the decodings results from this iteration of the foreach loop
    #DECODING_RESULTS
    
    all_cv_results <- dplyr::bind_rows(all_cv_results)
    
    rank_and_decision_val_results <- get_rank_results(all_cv_results)
    
    results <- cbind(all_cv_results, rank_and_decision_val_results)
    
    
    

    # mean_decoding_results <- results %>%
    #    group_by(train_time, test_time, CV) %>%
    #    summarize(zero_one_loss = mean(correct),
    #              normalized_rank = mean(normalized_rank_results),
    #              decision_vals = mean(correct_class_decision_val))


    mean_decoding_results <- results %>%
      dplyr::rename(test_time = time_bin) %>%
      group_by(train_time, test_time, CV) %>%
      summarize(zero_one_loss = mean(correct),
                normalized_rank = mean(normalized_rank_results),
                decision_vals = mean(correct_class_decision_val))
    
    
    
    
    
    
  }  # end loop over resample runs


  
  # return all the decoding results collapsed into one data frame
  dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  

}  # end the run_decoding method




