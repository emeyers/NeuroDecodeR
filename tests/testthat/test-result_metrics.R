

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
  
  
  #plot(aggregated_resample_rm)
  
}





test_that("confusion_matrix_RM the aggregate functions don't give an error", {
  
  cm_rm <- confusion_matrix_RM()    #, save_only_same_train_test_time = TRUE)
  
  test_aggregate_functions_work(cm_rm)
  
})




test_that("main_results_RM the aggregate functions don't give an error", {
  
  main_rm <- main_results_RM ()    #, save_only_same_train_test_time = TRUE)
  
  test_aggregate_functions_work(main_rm)
  
})





















