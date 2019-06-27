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
                                  .export=c('get_rank_results')) %do% {  # %dopar% {  
      
                                    
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
        
        
        #DECODING_RESULTS <- rbind(DECODING_RESULTS, curr_results)
        
        
      }   # end the for loop over time bins
      tictoc::toc()
  
      
      # oops, need to aggregate over CV splits too...
      all_cv_results[[iCV]] <- dplyr::bind_rows(all_time_results)
      
      
    }  # end the for loop over CV splits
  
  
    # close parallel resources
    doParallel::stopImplicitCluster()
    
    # return the decodings results from this iteration of the foreach loop
    #DECODING_RESULTS
    
    all_results <- dplyr::bind_rows(all_cv_results)
    
    rank_and_decision_val_results <- get_rank_results(all_results)
    
    results <- cbind(all_results, rank_and_decision_val_results)
    
    
    confusion_matrix <- get_confusion_matrix(all_results)
    

    # mean_decoding_results <- results %>%
    #    group_by(train_time, test_time, CV) %>%
    #    summarize(zero_one_loss = mean(correct),
    #              normalized_rank = mean(normalized_rank_results),
    #              decision_vals = mean(correct_class_decision_val))


    mean_decoding_results <- results %>%
      group_by(train_time, test_time, CV) %>%
      summarize(zero_one_loss = mean(correct),
                normalized_rank = mean(normalized_rank_results),
                decision_vals = mean(correct_class_decision_val))
    
    
    DECODING_RESULTS$mean_decoding_results <- mean_decoding_results
    DECODING_RESULTS$confusion_matrix <- confusion_matrix
    
    return(DECODING_RESULTS)
    
  }  # end loop over resample runs



  
  browser()
  
  mean_decoding_results <- purrr::map(ALL_DECODING_RESULTS, 'mean_decoding_results') 
  all_mean_decoding_results <- dplyr::bind_rows(mean_decoding_results, .id = "resample_run")

  
  confusion_matrices <- purrr::map(ALL_DECODING_RESULTS, 'confusion_matrix') 
  
  
  empty_cm <-  expand.grid(resample_run = "0",
                           train_time = unique(confusion_matrices[[1]]$train_time),
                           test_time = unique(confusion_matrices[[1]]$test_time),
                           actual_labels = unique(confusion_matrices[[1]]$actual_labels),
                           predicted_labels= unique(confusion_matrices[[1]]$predicted_labels),
                           n = 0, stringsAsFactors = FALSE)
  
  all_confusion_matrices <- dplyr::bind_rows(confusion_matrices, .id = "resample_run")
  
  all_confusion_matrices <- all_confusion_matrices
    dplyr::group_by(train_time,  test_time,   actual_labels,    predicted_labels) %>%
    summarize(n = sum(n))

  all_confusion_matrices %>%
    ggplot(aes(actual_labels, predicted_labels, fill = n)) +
    geom_tile() +
    facet_grid(train_time ~ test_time)
  
  
  #combined_confusion_matrix <- dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  
  
  
  # return all the decoding results collapsed into one data frame
  dplyr::bind_rows(ALL_DECODING_RESULTS, .id = "resample_run")
  

}  # end the run_decoding method




