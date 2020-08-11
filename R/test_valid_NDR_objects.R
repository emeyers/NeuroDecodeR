
# Functions to test that NDR objects have appropriate methods
# to implement the NDR interfaces.



#' Tests if a data frame is in valid raster format
#'
#' This function takes a data frame and tests that the data frame is in valid
#' raster format by checking that the data frame contains variables with the
#' appropriate names. If the data frame is not in correct raster format, an
#' error will be thrown that contains a message why the data is not in valid
#' raster format.
#'
#' @param raster_data A data frame that will be checked to see if it
#' is in valid raster format.
#'
#' @examples
#' # This is valid raster data so the function will return no error message
#' raster_dir_name <- file.path(
#'   system.file("extdata", package = "NeuroDecodeR"),
#'   "Zhang_Desimone_7object_raster_data_rda"
#' )
#' file_name <- "bp1001spk_01A_raster_data.rda"
#' raster_full_path <- file.path(raster_dir_name, file_name)
#'
#' test_valid_raster_format(raster_full_path)
#'
#'
#' # Binned data is not in raster format (it has an extra column called siteID) so
#' # checking if it is in raster format should return an error.
#'
#' binned_file_name <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NeuroDecodeR")
#' try(test_valid_raster_format(binned_file_name))
#' 
#' 
#' @export
test_valid_raster_format <- function(raster_data) {
  
  if (is.character(raster_data)) {

    # if a file name is given, load the raster data
    raster_data_object_name <- load(raster_data)

    # there should be only 1 object in the raster_file_name
    if (length(raster_data_object_name) != 1) {
      stop(paste(
        "Data not in valid raster format:",
        "raster data files must contain a single object that has the raster data."))
    }

    eval(parse(text = paste0("raster_data <- ", raster_data_object_name)))
  }


  # raster data needs to have a variables that start with "time" and with "labels"
  unique_prefixes <- sort(unique(sapply(strsplit(names(raster_data), "[.]"), function(x) x[1])))

  if (sum(unique_prefixes == "time") != 1) {
    stop(paste(
      "Data not in valid raster format:",
      "raster data must be a data frame that contains a variables that start with 'time.'"))
  }

  if (sum(unique_prefixes == "labels") != 1) {
    stop(paste(
      "Data not in valid raster format:",
      "raster data must be a data frame that contains a variables that start with 'labels.'"))
  }



  # only two additional variable prefixes names are allowed which are "site_info." and "trial_number"
  valid_raster_data_variable_names <- c("time", "labels", "site_info", "trial_number")

  invalid_variable_names <- setdiff(unique_prefixes, valid_raster_data_variable_names)

  if (length(invalid_variable_names) != 0) {
    stop(paste(
      "Data not in valid raster format:",
      "raster data must only contain variables names that start with 'labels.'",
      "'time.', 'site_info.' or 'trial_number'. The following are not valid variable",
      "names to have in data that is in raster format:", invalid_variable_names))
  }
  
  
  
  # make sure the class of raster_data is c("raster_data", "data.frame")
  if (!("raster_data" %in% class(raster_data))) {
    
    warning(paste("The class attribute of files raster_data format should be set to: \n",
                  'attr(raster_data, "class") <- c("raster_data", "data.frame")',
                  "\n otherwise the plot() function will not work correctly for raster data"))
  }
  
  
  
}







# The following functions will not be exported for now. If users want to
# create new NDR objects they can still use these internal functions to check
# that the objects they creatd conform to NDR interfaces.



# A function that checks if binned data is in a valid format
test_valid_binned_format <- function(binned_data) {
  
  if (is.character(binned_data)) {
    binned_data_object_name <- load(binned_data)

    # there should be only 1 object in the binned_file
    if (length(binned_data_object_name) != 1) {
      stop(paste(
        "Data not in valid binned format:",
        "binned data files must contain a single object that has the binned data."))
    }

    eval(parse(text = paste0("binned_data <- ", binned_data_object_name)))
  }


  # need to have a variables that start with "siteID", time" and "labels"
  unique_prefixes <- unique(sapply(strsplit(names(binned_data), "[.]"), function(x) x[1]))

  if (sum(unique_prefixes == "time") != 1) {
    stop(paste(
      "Data not in valid binned format:",
      "binned data must be a data frame that contains a variables that start with 'time.'"))
  }

  if (sum(unique_prefixes == "labels") != 1) {
    stop(paste(
      "Data not in valid binned format:",
      "binned data must be a data frame that contains a variables that start with 'labels.'"))
  }

  if (sum(unique_prefixes == "siteID") != 1) {
    stop(paste(
      "Data not in valid binned format:",
      "binned data must be a data frame that contains a variable called 'siteID'"))
  }



  # only two additional variable prefixes names are allowed which are "site_info" and "trial_number"
  valid_binned_data_variable_names <- c("siteID", "time", "labels", "site_info", "trial_number")

  invalid_variable_names <- setdiff(unique_prefixes, valid_binned_data_variable_names)

  if (length(invalid_variable_names) != 0) {
    stop(paste(
      "Data not in valid binned format:",
      "binned data must only contain variables names that start with 'siteID', labels.'",
      "'time.', 'site_info' or 'trial_number'. The following are not valid variable",
      "names to have in data that is in binned format:", invalid_variable_names))
  }
  
  
  # make sure the class of binned_data is c("binned_data", "data.frame")
  # if (!("binned_data" %in% class(binned_data))) {
  #  warning(paste("The class attribute of files binned_data format should be set to: \n",
  #                'attr(binned_data, "class") <- c("binned_data", "data.frame")'))
  # }
  
  
}







# A function that checks that a datasource conforms to the NDR interface
test_valid_datasource <- function(the_datasource) {

  # make sure the data source has a get_data() method
  test_has_method(the_datasource, "get_data")

  # make sure the get_data() function returns a data frame with required variables
  the_data <- get_data(the_datasource)

  # should have exactly one column name that matches exactly these variable names
  check_df_contains_variable(the_data, "train_labels", "get_data()")
  check_df_contains_variable(the_data, "test_labels", "get_data()")
  check_df_contains_variable(the_data, "time_bin", "get_data()")

  # should have one or more column names that start with these variables
  check_df_contains_variable(the_data, "site", "get_data()", FALSE, FALSE)
  check_df_contains_variable(the_data, "CV", "get_data()", FALSE, FALSE)

  test_get_parameters_method(the_datasource)
  
}




# A function that checks that a classifier conforms to the NDR interface
test_valid_classifier <- function(the_classifier) {


  # make sure the classifier has a get_predictions() method
  test_has_method(the_classifier, "get_predictions")

  
  # get data for testing
  example_data <- get_example_training_and_test_data()
  training_set <- example_data$training_set
  test_set <- example_data$test_set


  predictions_df <- get_predictions(the_classifier, training_set, test_set)

  # should have exactly one column name that matches exactly these variable names
  check_df_contains_variable(predictions_df, "test_time", "get_predictions()")
  check_df_contains_variable(predictions_df, "actual_labels", "get_predictions()")
  check_df_contains_variable(predictions_df, "predicted_labels", "get_predictions()")

  # should have one or more column names that start with this variable name
  check_df_contains_variable(predictions_df, "decision_vals", "get_predictions()", FALSE, FALSE)

  test_get_parameters_method(the_classifier)
  
}





# A function that checks that a feature preprocessor conforms to the NDR interface
test_valid_feature_preprocessor <- function(the_feature_preprocessor) {


  # make sure that the feature preprocessor has a preprocess_data() method
  test_has_method(the_feature_preprocessor, "preprocess_data")


  # get data for testing
  example_data <- get_example_training_and_test_data()
  training_set <- example_data$training_set
  test_set <- example_data$test_set

  processed_data <- preprocess_data(the_feature_preprocessor, training_set, test_set)

  # should have exactly one column name that matches exactly these variable names
  check_df_contains_variable(processed_data$training_set, "train_labels", "preprocess_data()")
  check_df_contains_variable(processed_data$test_set, "test_labels", "preprocess_data()")
  check_df_contains_variable(processed_data$test_set, "time_bin", "preprocess_data()")

  # should have one or more column names that start with this variable name
  check_df_contains_variable(processed_data$training_set, "site", "preprocess_data()", FALSE, FALSE)
  check_df_contains_variable(processed_data$test_set, "site", "preprocess_data()", FALSE, FALSE)

  test_get_parameters_method(the_feature_preprocessor)
  
}





# A function that checks that a cross-validator conforms to the NDR interface
test_valid_cross_validator <- function(the_cross_validator) {


  # make sure that the cross-validator has a run_decoding() method
  test_has_method(the_cross_validator, "run_decoding")


  # analysis_ID is not required for a cross-validator but should have one so
  # send a warning if it doesn't exist
  property_names <- names(the_cross_validator)
  if (!("analysis_ID" %in% property_names)) {
    
    warning(paste(
      "It is recommended that cross-validators have a property called",
      "analysis_ID take contains a unique identify for this analysis.",
      "See the generate_analysis_ID() in private_functions.R"))
  }


  test_get_parameters_method(the_cross_validator)
  
}





# A function that checks that a result metric conforms to the NDR interface
test_valid_result_metric <- function(the_result_metric) {

  # Check these methods exist. No requirements that they do anything useful
  test_has_method(the_result_metric, "aggregate_CV_split_results")
  test_has_method(the_result_metric, "aggregate_resample_run_results")

  test_get_parameters_method(the_result_metric)
  
}








# Helper functions for testing if data is in valid NDR format --------------


# A helper function to test if a given class has a method. This is useful to
# check that particular object types in the NDR fulfill the interface.
test_has_method <- function(ndr_object, method_name) {

  class_name <- class(ndr_object)[1]

  if (!(exists(paste(method_name, class_name, sep = "."), mode = "function"))) {
    stop(paste(class(ndr_object), "does not have a method called", method_name))
  }
  
}





# A function that checks that a get_parameters() method exists and returns
# a data frame with a single row.
test_get_parameters_method <- function(ndr_object) {
  
  test_has_method(ndr_object, "get_parameters")

  the_parameters <- get_parameters(ndr_object)

  if (!is.data.frame(the_parameters)) {
    stop(
      paste("The get_parameters method for", class(ndr_object)),
      "is not returning a data frame.")
  }

  if (dim(the_parameters)[1] != 1) {
    stop(
      paste("The get_parameters method for", class(ndr_object)),
      "must return a data frame with a *single* row.")
  }
  
}






# A helper function that checks if a data frame contains a variable and prints a
#  useful error message if the data frame does not contain the variable.
check_df_contains_variable <- function(df, variable_name, error_message_method_name,
                                       exact_variable_match = TRUE, exactly_one_match = TRUE) {
  
  df_variable_names <- names(df)

  if (exact_variable_match) {
    num_matches <- sum(variable_name %in% df_variable_names)
  } else {
    num_matches <- length(grep(variable_name, df_variable_names))
  }

  check_is_valid <- FALSE

  if (exactly_one_match) {
    check_is_valid <- num_matches == 1
  } else {
    check_is_valid <- num_matches > 0
  }

  # if the df does not contain the variable name as specified, print an error message
  if (!check_is_valid) {
    stop(paste0(
      "The data frame returned by the ", error_message_method_name,
      " method does not have the required variable name '", variable_name, "' as required."))
  }
  
}







# A helper function that generates data to test datasources and feature-preprocessors
get_example_training_and_test_data <- function() {
  
  real_data_binned_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), 
                                            package = "NeuroDecodeR")

  # generate shuffled count data...
  ds <- ds_basic(real_data_binned_file_name, "stimulus_ID",
    site_IDs_to_exclude = c(39, 63),
    num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)

  cv_data <- get_data(ds)

  training_set <- filter(cv_data, .data$time_bin == "time.200_349", .data$CV_1 == "train") %>%
    select(starts_with("site"), .data$train_labels)

  test_set <- filter(cv_data, .data$time_bin %in% c("time.-350_-201", "time.200_349"), .data$CV_1 == "test") %>%
    dplyr::select(starts_with("site"), .data$test_labels, .data$time_bin)

  levels(test_set$time_bin)[levels(test_set$time_bin) == "time.-350_-201"] <- "baseline"
  levels(test_set$time_bin)[levels(test_set$time_bin) == "time.200_349"] <- "stimulus"

  example_training_test_data <- list(training_set = training_set, test_set = test_set)

  example_training_test_data
  
}
