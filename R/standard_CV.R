#' The standard cross-validator (CV)  object
#'
#' A cross-validator object takes a datasource, feature preprocessors and a classifier
#' and runs multiple cross-validation cycles by getting new training and test data splits, 
#' running the preprocessor to do preprocessing of the data, trains and tests the classifier, and 
#' the creates metric to evaluation the classification performance on the test set.  
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section standard_CV:
#' 
#' \describe{
#' \item{\code{standard_CV$new(data_source, classifier, feature_preprocessors)  )}}{
#' If successful, will return a new \code{basic_DS} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{run_decoding}}{
#' The main method that runs the cross-validation analysis. 
#' }}
#' 
#' 
#' 
#' @import R6
#' @export

standard_CV <- R6Class("standard_CV", 
    public = list(
      # properties
      data_source = NA,
      classifier = NA,
      feature_preprocessors = NA,
      num_resample_runs = 50,
                      
      # constructor
      initialize = function(data_source, classifier, feature_preprocessors) {
        self$data_source <- data_source
        self$classifier <- classifier
        self$feature_preprocessors <- feature_preprocessors
      },
        
      # methods
      run_decoding = function(){
        data_source <- self$data_source
        classifier = self$classifier
        feature_preprocessors = self$feature_preprocessors
        num_resample_runs = self$num_resample_runs
        DECODING_RESULTS <- NULL
        
        # Add a loop over resample runs...

        # get the data from the current cross-validation run        
        cv_data <- data_source$get_data()
        
        unique_times <- unique(cv_data$time)
        num_time_bins <- length(unique_times)
        all_cv_train_test_inds <- select(cv_data, starts_with("CV"))
        num_CV <- ncol(all_cv_train_test_inds)

        # add names for the different dimensions of the results
        time_names <- grep("^time", names(data_source$binned_data), value = TRUE)
        dim_names <- list(1:num_CV, time_names, time_names)
        
        zero_one_loss_results <- array(NA, c(num_CV, num_time_bins, num_time_bins), dimnames = dim_names)
        decision_value_results <- array(NA, c(num_CV, num_time_bins, num_time_bins), dimnames = dim_names)
        rank_results <- array(NA, c(num_CV, num_time_bins, num_time_bins), dimnames = dim_names)
        
        for (iCV in 1:num_CV) {
          tic()
          print(iCV) 
          for (iTrain in 1:num_time_bins) {
            train_data <- filter(cv_data, time == unique_times[iTrain], all_cv_train_test_inds[iCV] == "train") %>% select(starts_with("site"), labels)
            test_data <- filter(cv_data, all_cv_train_test_inds[iCV] == "test") %>% select(starts_with("site"), labels, time)
            
            # if feature-processors have been specified, do feature processing...
            if (length(feature_preprocessors) > 1) {
              for (iFP in 1:length(feature_preprocessors)) {
                # get the preprocessed data...
                processed_data <- fps[[iFP]]$preprocess_data(train_data, test_data)
                # update the training and test data with this preprocessed data...
                train_data <- processed_data$train_data
                test_data <- processed_data$test_data
              }
            }  # end the if statement for doing preprocessing
            
            results <- classifier$get_predictions(train_data, test_data)
            # add more measures of decoding accuracy (rank results, etc)
            rank_and_decision_val_results <- private$get_rank_results(results)
            results <- cbind(results, rank_and_decision_val_results)

            # get the results averaged over all classes for each time period
            mean_decoding_results <- results %>% group_by(time) %>% 
              summarize(mean_accuracy = mean(correct), 
                        mean_rank = mean(normalized_rank_results),
                        mean_decision_vals = mean(correct_class_decision_val)
                        )
  
            zero_one_loss_results[iCV, iTrain, ] <- mean_decoding_results$mean_accuracy
            decision_value_results[iCV, iTrain, ] <- mean_decoding_results$mean_decision_vals 
            rank_results[iCV, iTrain, ] <- mean_decoding_results$mean_rank
          }   # end the for loop over time bins
          toc()
        }  # end the for loop over CV splits
        # combine all the results in one list to be returned
        DECODING_RESULTS$zero_one_loss_results <- zero_one_loss_results
        DECODING_RESULTS$decision_value_results <- decision_value_results
        DECODING_RESULTS$rank_results <- rank_results
        
        return(DECODING_RESULTS)
      }  # end the run_decoding method
  ),  # end the public methods

  # private methods
  private = list(
    # get the rank results and the decision value for predicted class...
    get_rank_results = function(results) {
      decision_vals <- select(results, starts_with("decision"))
      num_classes <- ncol(decision_vals)
      num_test_points <- nrow(decision_vals)
      # remove the prefix 'decision.vals' from the column names...
      the_names <- names(decision_vals)
      the_names <- unlist(strsplit(the_names, "decision_val_", fixed = TRUE))  
      the_names <- the_names[the_names != ""]
      names(decision_vals) <- the_names
      decision_vals_aug <- cbind(results$actual_labels, decision_vals)
      #i <- 1; decision_vals_aug_row <- decision_vals_aug [i, ]
    

      ####  Bad code - doesn't work with negative numbers because apply converts values to strings before sorting
      ### (and then the negative sign becomes a dash so it sorts the values in the wrong order)
      ##  # get the normalized rank results (the lines below are not working correcly with negative values...)      
      ##  get_rank_one_row <- function(decision_vals_aug_row) {
      ##    which(names(sort(decision_vals_aug_row[2:length(decision_vals_aug_row)], decreasing = TRUE)) == as.character(as.matrix(decision_vals_aug_row[1]))) 
      ##  }
       
  
      # This code is written less compactly but it works correctly with negative decision values
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
      

      
    ##  # much slower code (though potentially easier to read)
    ##  normalized_rank_results <- rep(NA, num_test_points)
    ##  correct_class_decision_val <- rep(NA, num_test_points)
    ##  for (iTestPoint in 1:num_test_points){
    ##    curr_sorted_decision_vals <- sort(decision_vals[iTestPoint, ], decreasing = TRUE) 
    ##    curr_rank_result <- which(names(curr_sorted_decision_vals) == results$actual_labels[iTestPoint])
    ##    normalized_rank_results[iTestPoint] <- 1 - ((curr_rank_result - 1)/(num_classes - 1))
    ##    correct_class_decision_val[iTestPoint] <- decision_vals[iTestPoint, which(results$actual_labels[iTestPoint] == the_names)]
    ##  }

      
      rank_and_decision_val_results <- data.frame(normalized_rank_results, correct_class_decision_val)
     }
  )  # end private properties/methods
) # end the class 














