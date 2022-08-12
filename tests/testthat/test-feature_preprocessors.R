

# load data for testing
load("example_ZD_train_and_test_set.Rda")
rm(count_training_set, count_test_set, normalized_training_set, normalized_test_set)



# tests for the fp_zscore -----------------------------------------------------


test_that("fp_zscore() conforms to the NDR datasouce interface", {
  fp <- fp_zscore()
  expect_null(test_valid_feature_preprocessor(fp))
})



test_that("fp_zscore normalized the training data so that it has a mean of 0 and a sd of 1", {
  
  fp <- fp_zscore()
  processed_data <- preprocess_data(fp, training_set, test_set)
  zscore_normalized_training_set <- processed_data$training_set
  zscore_normalized_test_set <- processed_data$test_set
  
  zscore_normalized_train_data <- dplyr::select(zscore_normalized_training_set, starts_with("site"))
  expect_equal(max(colSums(zscore_normalized_train_data)), 0)  # all means are 0
  expect_equal(max(abs(sapply(zscore_normalized_train_data, sd))), 1)  # all sd are 1 (or potentially less)
  
})




test_that("the fp_score constructor correctly adds objects to an ndr container", {
  
  raw_fp <- fp_zscore()
  expect_equal(class(raw_fp), "fp_zscore")
  
  fp_combined_with_an_cl <- raw_fp %>% cl_max_correlation()
  expect_equal(class(fp_combined_with_an_cl), "ndr_container")
  
  an_ndr_container <- ndr_container()
  fp_combined_with_container <- an_ndr_container %>% fp_zscore()
  expect_equal(class(fp_combined_with_container), "ndr_container")
  expect_equal(class(fp_combined_with_container$fp), "list")
  expect_equal(class(fp_combined_with_container$fp[[1]]), "fp_zscore")
  
  fp_combined_with_an_fp <- raw_fp %>% fp_zscore()
  expect_equal(class(fp_combined_with_an_fp), "ndr_container")
  expect_equal(class(fp_combined_with_an_fp$fp), "list")
  expect_equal(class(fp_combined_with_an_fp$fp[[1]]), "fp_zscore")
  
})





# fp_select_k_features --------------------------------------------------------

test_that("fp_select_k_features() conforms to the NDR datasouce interface", {
  fp <- fp_select_k_features(num_sites_to_use = 100)
  expect_null(test_valid_feature_preprocessor(fp))
})



test_that("fp_select_k_features p-values are correct", {
  
  fp <- fp_select_k_features(num_sites_to_use = 100)
  processed_data <- preprocess_data(fp, training_set, test_set)
  fp_pvals <- processed_data$fp_info$pvals

  
  # test the the ANOVA function that I will write works
  all_pvals <- NULL
  for (iSite in 1:(ncol(training_set) - 1)){
       curr_data <- training_set[, iSite][[1]]
       all_pvals[iSite] <- anova(lm(curr_data ~ training_set$train_labels))$Pr[1]
    }
  
  expect_equal(fp_pvals, all_pvals)  

})




test_that("fp_select_k_features returns the correct number of features", {
  
  fp <- fp_select_k_features(num_sites_to_use = 100)
  processed_data <- preprocess_data(fp, training_set, test_set)
  expect_equal(dim(select(processed_data$training_set, starts_with("site")))[2], 100)  
  expect_equal(dim(select(processed_data$test_set, starts_with("site")))[2], 100)  
  
  fp <- fp_select_k_features(num_sites_to_exclude = 100)
  processed_data <- preprocess_data(fp, training_set, test_set)
  expected_num_sites <- dim(select(training_set, starts_with("site")))[2] - 100
  expect_equal(dim(select(processed_data$training_set, starts_with("site")))[2], expected_num_sites)  
  expect_equal(dim(select(processed_data$test_set, starts_with("site")))[2], expected_num_sites)  
  

  num_sites_to_use <- 50
  num_sites_to_exclude <- 10
  fp <- fp_select_k_features(num_sites_to_use = num_sites_to_use, 
                             num_sites_to_exclude = num_sites_to_exclude)
  processed_data <- preprocess_data(fp, training_set, test_set)
  expect_equal(dim(select(processed_data$training_set, starts_with("site")))[2], 50)  
  expect_equal(dim(select(processed_data$test_set, starts_with("site")))[2], 50)  
  
  ordered_sites <- arrange(processed_data$fp_info, pvals)
  expect_equal(sum(ordered_sites$selected_site[1:num_sites_to_exclude]), 0)
  expect_equal(sum(ordered_sites$selected_site), num_sites_to_use)
  
})





test_that("the fp_score constructor correctly adds objects to an ndr container", {
  
  raw_fp <- fp_select_k_features(num_sites_to_use = 100)
  expect_equal(class(raw_fp), "fp_select_k_features")
  
  fp_combined_with_an_cl <- raw_fp %>% cl_max_correlation()
  expect_equal(class(fp_combined_with_an_cl), "ndr_container")
  
  an_ndr_container <- ndr_container()
  fp_combined_with_container <- an_ndr_container %>% fp_select_k_features(num_sites_to_use = 100)
  expect_equal(class(fp_combined_with_container), "ndr_container")
  expect_equal(class(fp_combined_with_container$fp), "list")
  expect_equal(class(fp_combined_with_container$fp[[1]]), "fp_select_k_features")
  
  fp_combined_with_an_fp <- raw_fp %>% fp_select_k_features(num_sites_to_use = 100)
  expect_equal(class(fp_combined_with_an_fp), "ndr_container")
  expect_equal(class(fp_combined_with_an_fp$fp), "list")
  expect_equal(class(fp_combined_with_an_fp$fp[[1]]), "fp_select_k_features")
  
})




