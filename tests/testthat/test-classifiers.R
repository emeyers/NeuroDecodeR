

# source('generate_data_to_test_NDTr.R')


load("example_ZD_train_and_test_set.Rda")
rm(training_set, test_set, count_training_set, count_test_set)


test_that("classification results seem reasonable", {
  
  cl <- max_correlation_CL()
  
  prediction_results <- get_predictions(cl, normalized_training_set, normalized_test_set)
  
  accuracies <- prediction_results %>%
    dplyr::group_by(test_time) %>%
    dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))

  
  expect_gt(filter(accuracies, test_time == "stimulus")$mean_accuracy, .6)
  expect_lt(filter(accuracies, test_time == "baseline")$mean_accuracy, .3)
  

})






test_that("classification results on shuffled data are around chance", {
  
  cl <- max_correlation_CL()
  
  prediction_results <- get_predictions(cl, shuffled_normalized_training_set, shuffled_normalized_test_set)
  
  accuracies <- prediction_results %>%
    dplyr::group_by(test_time) %>%
    dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))
  
  
  expect_lt(filter(accuracies, test_time == "stimulus")$mean_accuracy, .3)
  expect_lt(filter(accuracies, test_time == "baseline")$mean_accuracy, .3)
  
  
})


