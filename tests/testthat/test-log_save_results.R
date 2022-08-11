

# initial setup --------------------------------------------------------------


# load the test data
load("example_DECODING_RESULTS.Rda")

results_dir_name <- trimws(file.path(tempdir(), "testing_save_results", " "))


# delete any saved results and manifest files that already exist
if (file.exists(file.path(dirname(results_dir_name), basename(results_dir_name)))) {
  
  the_files <- paste0(results_dir_name, list.files(results_dir_name))
  #file.remove(the_files)
  unlink(file.path(dirname(the_files), basename(the_files)), recursive = TRUE)
  
} else {
  
  dir.create(results_dir_name)
  
}





test_that("log_save_results can save results and create a manifest file", {
  
  # add an intial result to the manifest file and save it
  # should get a warning that the manifest file does not exist
  expect_warning(log_save_results(DECODING_RESULTS_1, results_dir_name, "results 1"))
  
  # add a second result, should not get a warning
  log_save_results(DECODING_RESULTS_2, results_dir_name, "save results 2")
  
  # if the second result is added again should get a warning that there are duplicate results
  expect_warning(log_save_results(DECODING_RESULTS_2, results_dir_name, "results 2 again"))
  
  
})





test_that("log_check_results_already_exist() can correctly assess which results have been saved", {
  
  # get the parameters from exisiting results
  test_params_1 <- get_parameters(DECODING_RESULTS_1$cross_validation_paramaters)
  test_params_2 <- get_parameters(DECODING_RESULTS_2$cross_validation_paramaters)
  test_params_3 <- get_parameters(DECODING_RESULTS_3$cross_validation_paramaters)
  test_params_4 <- get_parameters(DECODING_RESULTS_4$cross_validation_paramaters)
  test_params_5 <- get_parameters(DECODING_RESULTS_5$cross_validation_paramaters)
  
  # the parameters/results from DECODING_RESULTS_1 should be saved
  load(paste0(results_dir_name, "results_manifest.rda"))
  expect_true(log_check_results_already_exist(test_params_1, manifest_df))
  
  # the parameters/results from DECODING_RESULTS_3 should not exist                                   
  expect_false(log_check_results_already_exist(test_params_3, manifest_df))
  
  # add results 3 
  log_save_results(DECODING_RESULTS_3, results_dir_name, "results 3")

  # now results 3 should exist
  load(paste0(results_dir_name, "results_manifest.rda"))
  expect_true(log_check_results_already_exist(test_params_3, manifest_df))
  
  # result 4 is the same as result 3 but only using siteIDs 1-100
  #  this should be noted as different (siteIDs is stored in a list
  #  in the manifest file so making sure comparing lists works)
  expect_false(log_check_results_already_exist(test_params_4, manifest_df))
  

})
  



  
test_that("log_load_results_from_params() can load saved results based on decoding parameters", {
  
  
  test_params_1 <- get_parameters(DECODING_RESULTS_1$cross_validation_paramaters)
  test_params_2 <- get_parameters(DECODING_RESULTS_2$cross_validation_paramaters)
  test_params_5 <- get_parameters(DECODING_RESULTS_5$cross_validation_paramaters)
  
  
  # should return a single DECODING_RESULTS 
  decoding_results_1 <- log_load_results_from_params(test_params_1, results_dir_name)

  # this is the usual DECODING_RESULTS which is a named list
  expect_named(decoding_results_1)
  
  # should return a list of DECODING_RESULTS since the data was saved twice
  decoding_results_2 <- log_load_results_from_params (test_params_2, results_dir_name)
  
  # the names here are null since this is a list of decoding results
  expect_null(names(decoding_results_2))
  
  # result 5 is the same as result 3 even though it was run at a different time
  # should still load result 3 using result 5 parameters (even though result 5
  # has not been added, if it had then a list of results would be returned)
  decoding_results_3_and_5 <- log_load_results_from_params (test_params_5, results_dir_name)
  
})





test_that("log_load_results_from_result_name() can load saved results based on the result_name", {
  
  # should return a single DECODING_RESULTS which is a named list
  decoding_retrieved_1 <- log_load_results_from_result_name("results 1", results_dir_name)
  expect_named(decoding_retrieved_1)
  
  
  # should return a list of DECODING_RESULTS since the data was saved twice
  decoding_retrieved_2 <- log_load_results_from_result_name("results .*", results_dir_name)
  
  
  # the names here are null since this is a list of decoding results
  expect_null(names(decoding_retrieved_2))
  
  
  # expect an error when trying to get a result for a name that does not exist
  expect_error(log_load_results_from_result_name("e = 2r", results_dir_name))
  
  
})




# doing this test here since I have multiple results saved
test_that("Test that can plot several results using plot_main_results (doing this in test-log_save_results", {
  
  expect_visible(plot_main_results(results_dir_name, 1:2, display_names = c("first result", "second result")))
  
  expect_visible(plot_main_results(results_dir_name, c("results 1", "results 2 again")))
  
})




# clean up by deleting any saved results and manifest file
the_files <- paste0(results_dir_name, list.files(results_dir_name))
file.remove(the_files)
unlink(file.path(dirname(results_dir_name), basename(results_dir_name)), recursive = TRUE, force = TRUE)




