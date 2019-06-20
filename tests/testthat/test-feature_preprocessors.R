

# load data for testing
load("example_ZD_train_and_test_data.Rda")
rm(count_train_data, count_test_data, normalized_train_data, normalized_test_data)



test_that("zscore_FP normalized the training data so that it has a mean of 0 and a sd of 1", {
  
  fp <- zscore_FP()
  processed_data <- preprocess_data(fp, train_data, test_data)
  zscore_normalized_train_data <- processed_data$train_data
  zscore_normalized_test_data <- processed_data$test_data
  
  normalized_train_data <- dplyr::select(zscore_normalized_train_data, starts_with("site"))
  expect_equal(max(colSums(zscore_normalized_train_data)), 0)  # all means are 0
  expect_equal(max(abs(sapply(zscore_normalized_train_data, sd) - 1)) - 1, 0)  # all sd are 1
  
})

# can't really check the test data because it won't be exactly equal to 0 or have an sd of 1...






