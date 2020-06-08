

# source("tests/testthat/test_valid_NDTr_objects.R")

# source('generate_data_to_test_NDTr.R')


load("example_ZD_train_and_test_set.Rda")
rm(training_set, test_set)





# helper test functions 

test_reasonable_classification_accuracy <- function(cl){
  
  test_that("testing classification results seem reasonable", {
    
    # prediction_results <- get_predictions(cl, normalized_training_set, normalized_test_set)
    prediction_results <- get_predictions(cl, count_training_set, count_test_set)
    
    accuracies <- prediction_results %>%
      dplyr::group_by(test_time) %>%
      dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))
    
    expect_gt(dplyr::filter(accuracies, test_time == "stimulus")$mean_accuracy, .53)
    expect_lt(dplyr::filter(accuracies, test_time == "baseline")$mean_accuracy, .3)
    
  })
  
}


test_shuffle_results_at_chance <- function(cl){
  
  test_that("testing classification results on shuffled data are around chance", {
    
    #prediction_results <- get_predictions(cl, shuffled_normalized_training_set, shuffled_normalized_test_set)
    prediction_results <- get_predictions(cl, shuffled_count_training_set, shuffled_count_test_set)
    
    accuracies <- prediction_results %>%
      dplyr::group_by(test_time) %>%
      dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))
    
    expect_lt(dplyr::filter(accuracies, test_time == "stimulus")$mean_accuracy, .3)
    expect_lt(dplyr::filter(accuracies, test_time == "baseline")$mean_accuracy, .3)
  })
}
  
  

# test cl_max_correlation -----------------------------------------------------

# test classifier has required methods and returns correctly formatted results
cl <- cl_max_correlation()
test_valid_classifier(cl) 

test_reasonable_classification_accuracy(cl)
test_shuffle_results_at_chance(cl)




# test cl_poison_naive_bayes --------------------------------------------------

cl <- cl_poisson_naive_bayes()
test_valid_classifier(cl) 

test_reasonable_classification_accuracy(cl)
test_shuffle_results_at_chance(cl)



# test cl_svm -----------------------------------------------------------------
cl <- cl_svm()
test_valid_classifier(cl) 
test_reasonable_classification_accuracy(cl)
test_shuffle_results_at_chance(cl)


cl <- cl_svm(kernel = "linear")
test_valid_classifier(cl) 
test_reasonable_classification_accuracy(cl)
test_shuffle_results_at_chance(cl)







