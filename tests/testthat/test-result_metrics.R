

# get some data...
load("cv_results_example.Rda")



# a helper function to test the aggregate_CV_split_results() and 
#   aggregate_resample_run_results() don't give errors
test_aggregate_functions_work <- function(result_metric_obj){
  
  # check aggregate_CV_split_results() doesn't give an error
  aggregated_cv_rm <- aggregate_CV_split_results(result_metric_obj, all_cv_results)

  
  # while actually not completely required, in general aggregator functions should 
  #  reduce the number of rows in the data
  expect_lte(dim(aggregated_cv_rm)[1], dim(all_cv_results)[1])
  
  
  # combine the data twice to create data from 2 resample runs
  fake_resample_run_data <- rbind(aggregated_cv_rm, aggregated_cv_rm)
  
  # check aggregate_resample_run_results() doesn't give an error
  aggregated_resample_rm <- aggregate_resample_run_results(fake_resample_run_data)
  
  
  # only will reduce size of data if I have data from multiple CV runs...
  expect_lte(dim(aggregated_resample_rm)[1], dim(fake_resample_run_data)[1])
  
  
}






test_that("rm_main_results aggregate methods don't give errors", {
  
  main_rm <- rm_main_results()    #, save_only_same_train_test_time = TRUE)
  
  expect_null(test_valid_result_metric(main_rm))
  
  test_aggregate_functions_work(main_rm)
  
  main_rm_2 <- rm_main_results(NULL, include_norm_rank_results = "only_same_train_test_time")  
  test_aggregate_functions_work(main_rm_2)
  
  main_rm_3 <- rm_main_results(NULL, FALSE)  
  test_aggregate_functions_work(main_rm_3)
  
})



test_that("the rm_main_results constructor correctly adds objects to an ndr container", {
  
  raw_rm <- rm_main_results()
  expect_equal(class(raw_rm)[1], "rm_main_results")
  
  rm_combined_with_an_cl <- raw_rm %>% cl_max_correlation()
  expect_equal(class(rm_combined_with_an_cl), "ndr_container")
  
  an_ndr_container <- ndr_container()
  rm_combined_with_container <- an_ndr_container %>% rm_main_results()
  expect_equal(class(rm_combined_with_container), "ndr_container")
  expect_equal(class(rm_combined_with_container$rm), "list")
  expect_equal(class(rm_combined_with_container$rm[[1]])[[1]], "rm_main_results")
  
  rm_combined_with_an_rm <- raw_rm %>% rm_main_results()
  expect_equal(class(rm_combined_with_an_rm), "ndr_container")
  expect_equal(class(rm_combined_with_an_rm$rm), "list")
  expect_equal(class(rm_combined_with_an_rm$rm[[1]])[[1]], "rm_main_results")
  
})






test_that("rm_confusion_matrix aggregate methods don't give errors", {
  
  cm_rm <- rm_confusion_matrix()    
  
  expect_null(test_valid_result_metric(cm_rm))
  
  test_aggregate_functions_work(cm_rm)
  
})





test_that("the rm_confusion_matrix constructor correctly adds objects to an ndr container", {
  
  raw_rm <- rm_confusion_matrix()
  expect_equal(class(raw_rm)[1], "rm_confusion_matrix")
  
  rm_combined_with_an_cl <- raw_rm %>% cl_max_correlation()
  expect_equal(class(rm_combined_with_an_cl), "ndr_container")
  
  an_ndr_container <- ndr_container()
  rm_combined_with_container <- an_ndr_container %>% rm_confusion_matrix()
  expect_equal(class(rm_combined_with_container), "ndr_container")
  expect_equal(class(rm_combined_with_container$rm), "list")
  expect_equal(class(rm_combined_with_container$rm[[1]])[[1]], "rm_confusion_matrix")
  
  rm_combined_with_an_rm <- raw_rm %>% rm_confusion_matrix()
  expect_equal(class(rm_combined_with_an_rm), "ndr_container")
  expect_equal(class(rm_combined_with_an_rm$rm), "list")
  expect_equal(class(rm_combined_with_an_rm$rm[[1]])[[1]], "rm_confusion_matrix")
  
})




