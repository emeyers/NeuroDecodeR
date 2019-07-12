


# initial setup --------------------------------------------------------------


# load the test data
load("example_DECODING_RESULTS.Rda")

results_dir_name <- file.path("testing_save_results", "")

if (file.exists(results_dir_name)){
  # delete any saved results and manifest files that already exist
  the_files <- paste0(results_dir_name, list.files(results_dir_name))
  file.remove(the_files)
} 

dir.create(results_dir_name)






test_that("Can save results and create a manifest file", {
  
  # add an intial result to the manifest file and save it
  # should get a warning that the manifest file does not exist
  expect_warning(save_and_log_results(DECODING_RESULTS_1, results_dir_name))
  
  # add a second result, should not get a warning
  save_and_log_results(DECODING_RESULTS_2, results_dir_name)
  
  
  # if the second result is added again should get a warning that there are duplicate results
  expect_warning(save_and_log_results(DECODING_RESULTS_2, results_dir_name))
  
  
})



test_that("Can correctly assess which results have been saved", {
  
  # get the parameters from exisiting results
  test_params_1 <- get_parameters(DECODING_RESULTS_1$cross_validation_paramaters)
  test_params_2 <- get_parameters(DECODING_RESULTS_2$cross_validation_paramaters)
  test_params_3 <- get_parameters(DECODING_RESULTS_3$cross_validation_paramaters)
  test_params_4 <- get_parameters(DECODING_RESULTS_4$cross_validation_paramaters)
  test_params_5 <- get_parameters(DECODING_RESULTS_5$cross_validation_paramaters)
  
  # the parameters/results from DECODING_RESULTS_1 should be saved
  load(paste0(results_dir_name, "results_manifest.rda"))
  expect_true(check_results_already_exist(test_params_1, manifest_df))
  
  # the parameters/results from DECODING_RESULTS_3 should not exist                                   
  expect_false(check_results_already_exist(test_params_3, manifest_df))
  
  # add results 3 
  save_and_log_results(DECODING_RESULTS_3, results_dir_name)

  # now results 3 should exist
  load(paste0(results_dir_name, "results_manifest.rda"))
  expect_true(check_results_already_exist(test_params_3, manifest_df))
  
  # result 4 is the same as result 3 but only using siteIDs 1-100
  #  this should be noted as different (siteIDs is stored in a list
  #  in the manifest file so making sure comparing lists works)
  expect_false(check_results_already_exist(test_params_4, manifest_df))
  
  
  
})
  



  
test_that("Can load saved results", {
  

  # should return a single DECODING_RESULTS 
  decoding_results_1 <- load_decoding_results(test_params_1, results_dir_name)

  # this is the usual DECODING_RESULTS which is a named list
  expect_named(decoding_results_1)
  
  # should return a list of DECODING_RESULTS since the data was saved twice
  decoding_results_2 <- load_decoding_results(test_params_2, results_dir_name)
  
  # the names here are null since this is a list of decoding results
  expect_null(names(decoding_results_2))
  
  # result 5 is the same as result 3 even though it was run at a different time
  # should still load result 3 using result 5 parameters
  decoding_results_3_and_5 <- load_decoding_results(test_params_5, results_dir_name)
  
})




# clean up by deleting any saved results and manifest file
the_files <- paste0(results_dir_name, list.files(results_dir_name))
file.remove(the_files)






