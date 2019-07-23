

# helper test functions to make sure that the objects in the NDTr 
#  have appropriate methods to implement their interfaces


the_package_name = "package:NDTr"




# a function that assess if raster data is in a valid format
test_valid_raster_data_format <- function(raster_file_name) {
  
  load(raster_file_name)
  
  expect_true(exists("raster_site_info"))
  expect_true(exists("raster_data"))
  
  unique_prefixes <- sort(unique(sapply(strsplit(names(raster_data), '[.]'), function(x) x[1])))
  
  expect_equal(unique_prefixes[1:2], c("labels", "time"))  
  
}




# a function that assess if binned data is in a valid format
test_valid_binned_data_format <- function(binned_file_name) {
  
  load(binned_file_name)
  
  expect_true(exists("binned_site_info"))
  expect_true(exists("binned_data"))
  
  unique_prefixes <- unique(sapply(strsplit(names(binned_data), '[.]'), function(x) x[1]))
  
  expect_equal(unique_prefixes, c("siteID", "labels", "time"))  
  
}






test_get_parameters_method <- function(NDTr_object){
  
  test_that("the get_parameters methods exists and returns a data frame with one row", {
    
    class_name <- class(NDTr_object)[1]
    
    package_with_method <- find(paste0("get_parameters.", class_name))
    expect_equal(package_with_method, the_package_name)
    
    the_parameters <- get_parameters(NDTr_object)
    expect_equal(dim(the_parameters)[1], 1)
    
  })
  
}




test_valid_datasource <- function(the_datasource){
  
  class_name <- class(the_datasource)[1]
  
  test_that("the datasource has get_data() method", {
  
    package_with_method <- find(paste0("get_data.", class_name))
    expect_equal(package_with_method, the_package_name)
    
  })
  

  test_that("get_data() returns a data frame with the appropriate variables", {
    
    # make sure the get_data() function returns a data frame with required variables
    the_data <- get_data(the_datasource)
    variable_names <- names(the_data)
  
    expect_true("train_labels" %in% variable_names)
    expect_true("test_labels" %in% variable_names)
    expect_true("time_bin" %in% variable_names)
    expect_gt(length(grep("site", variable_names)), 0)
    expect_gt(length(grep("CV", variable_names)), 0)
    
  })
  
  
  test_get_parameters_method(the_datasource)
  
}






test_valid_classifier <- function(the_classifier){
  
  
  # make sure the classifier has a get_predictions method
  class_name <- class(the_classifier)[1]
  
  test_that("the classifier has get_predictions() method", {
    package_with_method <- find(paste0("get_predictions.", class_name))
    expect_equal(package_with_method, the_package_name)
  })
  
  
  
  load("example_ZD_train_and_test_set.Rda")
  rm(training_set, test_set)
  
  test_that("classification results are in the correct format", {
    
    #predictions_df <- get_predictions(the_classifier, normalized_training_set, normalized_test_set)
    predictions_df <- get_predictions(the_classifier, count_training_set, count_test_set)
    
    non_decision_val_df <- select(predictions_df, -starts_with("decision_vals."))
    prediction_col_names <- sort(names(non_decision_val_df))
    expected_col_names <- sort(c("test_time", "actual_labels", "predicted_labels"))
    expect_equal(prediction_col_names, expected_col_names)
    
  })
  
  test_get_parameters_method(the_classifier)
  
}






test_valid_feature_preprocessor <- function(the_feature_preprocessor){
  

  test_that("the preprocessor has a preprocess_data() method", {
    
    class_name <- class(the_feature_preprocessor)[1]
    
    package_with_method <- find(paste0("preprocess_data.", class_name))
    expect_equal(package_with_method, the_package_name)
    
  })
  
  
  # check that preprocessed_data returns:
  #  1) processed_data$training_set which has site_X's and labels
  #  2) processed_data$test_set which has site_X's, labels and time_bin
  
  test_that("the preprocess_data() results in the correct format", {
    
    # load data for testing
    load("example_ZD_train_and_test_set.Rda")
    rm(count_training_set, count_test_set, normalized_training_set, normalized_test_set)
    
    processed_data <- preprocess_data(the_feature_preprocessor, training_set, test_set)
    
    training_set_names <- names(processed_data$training_set)
    expect_true("train_labels" %in% training_set_names)
    expect_gt(length(grep("site", training_set_names)), 0)

    test_set_names <- names(processed_data$test_set)
    expect_true("test_labels" %in% test_set_names)
    expect_gt(length(grep("site", test_set_names)), 0)
    expect_true("time_bin" %in% test_set_names)
    

  })
  
  test_get_parameters_method(the_feature_preprocessor)

}





test_valid_cross_validator <- function(the_cross_validator){
  
  
  test_that("the cross-validator has a run_decoding() method", {
    
    class_name <- class(the_cross_validator)[1]
    
    package_with_method <- find(paste0("run_decoding.", class_name))
    expect_equal(package_with_method, the_package_name)
    
  })
  
  
  
  # analysis_ID is not required for a cross-validator but should have one so
  # send a warning if it doesn't exist
  property_names <- names(the_cross_validator)
  if (!("analysis_ID" %in% property_names)) {
    warning(paste("It is recommended that cross-validators have a property called", 
                  "analysis_ID take contains a unique identify for this analysis.", 
                  "See the generate_analysis_ID() in private_functions.R"))
  }
  
  
  test_get_parameters_method(the_cross_validator)
  
}




test_valid_result_metric <- function(the_result_metric){
  
  
  # Check these methods exist. No requirements that they do anything useful
  test_that(paste("the result metric has aggregate_CV_split_results() and", 
            "aggregate_resample_run_results() methods"), {
    
    class_name <- class(the_result_metric)[1]
              
    package_with_method <- find(paste0("aggregate_CV_split_results.", class_name))
    expect_equal(package_with_method, the_package_name)
    
    package_with_method <- find(paste0("aggregate_resample_run_results.", class_name))
    expect_equal(package_with_method, the_package_name)
    
  })
  
  
  test_get_parameters_method(the_result_metric)
  
}










