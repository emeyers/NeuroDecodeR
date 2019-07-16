

# load data for testing
load("example_ZD_train_and_test_set.Rda")
rm(count_training_set, count_test_set, normalized_training_set, normalized_test_set)


# test that the feature processor conforms to the interface
fp <- fp_zscore()
test_valid_feature_preprocessor(fp)


test_that("fp_zscore normalized the training data so that it has a mean of 0 and a sd of 1", {
  
  fp <- fp_zscore()
  processed_data <- preprocess_data(fp, training_set, test_set)
  zscore_normalized_training_set <- processed_data$training_set
  zscore_normalized_test_set <- processed_data$test_set
  
  zscore_normalized_train_data <- dplyr::select(zscore_normalized_training_set, starts_with("site"))
  expect_equal(max(colSums(zscore_normalized_train_data)), 0)  # all means are 0
  expect_equal(max(abs(sapply(zscore_normalized_train_data, sd))), 1)  # all sd are 1 (or potentially less)
  
})

# can't really check the test data because it won't be exactly equal to 0 or have an sd of 1...






