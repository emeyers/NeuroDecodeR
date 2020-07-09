

# Functions to make sure that the objects in the NDTr have appropriate methods
# to implement the NDTr interfaces.




# A function that checks if raster data is in a valid format
test_valid_raster_data_format <- function(raster_file_name) {
  
  raster_data_object_name <- load(raster_file_name)
  # expect_true(exists("raster_data"))

  # there should be only 1 object in the binned_file
  expect_equal((length(raster_data_object_name)), 1)
  eval(parse(text = paste0("raster_data <- ", raster_data_object_name)))
  
  # needs to have a variables that start with "time" and with "labels"
  unique_prefixes <- sort(unique(sapply(strsplit(names(raster_data), '[.]'), function(x) x[1])))
  expect_equal(sum(unique_prefixes == "time"), 1)
  expect_equal(sum(unique_prefixes == "labels"), 1)
  
  # if a third type of variable is defined it should be called "site_info"
  if (length(unique_prefixes) == 3) {
    expect_equal(sum(unique_prefixes == "site_info"), 1)
  }
  
  # should only be three types of variables that start with labels, time or site info
  expect_true(length(unique_prefixes) <= 3)
  
}





# A function that checks if binned data is in a valid format
test_valid_binned_data_format <- function(binned_file_name) {
  
  binned_data_object_name <- load(binned_file_name)
  # expect_true(exists("binned_data"))
  
  # there should be only 1 object in the binned_file
  expect_equal((length(binned_data_object_name)), 1)
  eval(parse(text = paste0("binned_data <- ", binned_data_object_name)))
  
  
  # needs to have a variables that start with "siteID", time" and "labels"
  unique_prefixes <- unique(sapply(strsplit(names(binned_data), '[.]'), function(x) x[1]))
  expect_equal(sum(unique_prefixes == "time"), 1)
  expect_equal(sum(unique_prefixes == "labels"), 1)
  expect_equal(sum(unique_prefixes == "siteID"), 1)
  
  
  # if a third type of variable is defined it should be called "site_info"
  if (length(unique_prefixes) == 4) {
    expect_equal(sum(unique_prefixes == "site_info"), 1)
  }
  
  # should only be four types of variables that start with siteDi, labels, time or site_info
  expect_true(length(unique_prefixes) <= 4)

}





# A function that checks that a get_parameters() method exists
test_get_parameters_method <- function(NDTr_object){
  
  test_that("the get_parameters methods exists and returns a data frame with one row", {
    
    class_name <- class(NDTr_object)[1]
    
    helper_test_has_method(class_name, "get_parameters")
    
    the_parameters <- get_parameters(NDTr_object)
    expect_equal(dim(the_parameters)[1], 1)
    
  })
  
}






# A function that checks that a datasource conforms to the NDTr interface
test_valid_datasource <- function(the_datasource){
  
  class_name <- class(the_datasource)[1]
  
  test_that("the datasource has get_data() method", {
  
    helper_test_has_method(class_name, "get_data")
    
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






# A function that checks that a classifier conforms to the NDTr interface
test_valid_classifier <- function(the_classifier){
  
  
  # make sure the classifier has a get_predictions method
  class_name <- class(the_classifier)[1]
  
  test_that("the classifier has get_predictions() method", {
    
    helper_test_has_method(class_name, "get_predictions")

  })
  
  
  # get data for testing
  example_data <- helper_get_example_training_and_test_data()
  training_set <- example_data$training_set
  test_set <- example_data$test_set
  
  test_that("classification results are in the correct format", {
    
    predictions_df <- get_predictions(the_classifier, training_set, test_set)
    
    non_decision_val_df <- select(predictions_df, -starts_with("decision_vals."))
    prediction_col_names <- sort(names(non_decision_val_df))
    expected_col_names <- sort(c("test_time", "actual_labels", "predicted_labels"))
    expect_equal(prediction_col_names, expected_col_names)
    
  })
  
  test_get_parameters_method(the_classifier)
  
}





# A function that checks that a feature preprocessor conforms to the NDTr interface
test_valid_feature_preprocessor <- function(the_feature_preprocessor){
  
  
  test_that("the preprocessor has a preprocess_data() method", {
    
    class_name <- class(the_feature_preprocessor)[1]
    
    helper_test_has_method(class_name, "preprocess_data")
    
  })
  
  
  # check that preprocessed_data returns:
  #  1) processed_data$training_set which has site_X's and labels
  #  2) processed_data$test_set which has site_X's, labels and time_bin
  
  test_that("the preprocess_data() results in the correct format", {
    
    # get data for testing
    example_data <- helper_get_example_training_and_test_data()
    training_set <- example_data$training_set
    test_set <- example_data$test_set
    
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






# A function that checks that a cross-validator conforms to the NDTr interface
test_valid_cross_validator <- function(the_cross_validator){
  
  test_that("the cross-validator has a run_decoding() method", {
    
    class_name <- class(the_cross_validator)[1]
    
    helper_test_has_method(class_name, "run_decoding")
    
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






# A function that checks that a result metric conforms to the NDTr interface
test_valid_result_metric <- function(the_result_metric){
  
  # Check these methods exist. No requirements that they do anything useful
  test_that(paste("the result metric has aggregate_CV_split_results() and", 
            "aggregate_resample_run_results() methods"), {
    
    class_name <- class(the_result_metric)[1]
              
    helper_test_has_method(class_name, "aggregate_CV_split_results")
    helper_test_has_method(class_name, "aggregate_resample_run_results")
    
  })
  
  
  test_get_parameters_method(the_result_metric)
  
}





# A helper function to test if a given class has a method this is useful to
# check that particular object types in the NDTr fulfill the interface.
helper_test_has_method <- function(class_name, method_name){
  
  expect_true(exists(paste(method_name, class_name, sep = "."), mode='function'))
  
}





# A helper function that generates data to test datasources and feature-preprocessors 
helper_get_example_training_and_test_data <- function() {
  
  real_data_binned_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package="NDTr")

  # generate shuffled count data...
  ds <- ds_basic(real_data_binned_file_name, "stimulus_ID", site_IDs_to_exclude = c(39, 63),
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
  
  cv_data <- get_data(ds)
  
  training_set <- filter(cv_data, time_bin == "time.200_349", CV_1 == "train") %>% 
    select(starts_with("site"), train_labels)
  
  test_set <- filter(cv_data, time_bin %in% c("time.-350_-201", "time.200_349"), CV_1 == "test") %>% 
    select(starts_with("site"), test_labels, time_bin)
  
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-201"] <- "baseline"
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_349"] <- "stimulus"

  example_training_test_data <- list(training_set = training_set, test_set = test_set)
  
  example_training_test_data

}



