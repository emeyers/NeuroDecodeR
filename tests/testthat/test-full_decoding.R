




# An integration test that one can run a simple decoding analysis

test_that("cv_standard can run simple decoding analysis", {

  basedir_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), 
                                   package="NeuroDecodeR")
  
  ds <- ds_basic(basedir_file_name, 'stimulus_ID', 6, num_label_repeats_per_cv_split = 3)
  fps <- list(fp_zscore(), fp_select_k_features(num_site_to_use = 100))
  cl <- cl_max_correlation()
  rms <- list(rm_main_results(aggregate_decision_values = "diag", aggregate_normalized_rank = "diag"), 
            rm_confusion_matrix(save_only_same_train_test_time = FALSE))
  
  
  cv <- cv_standard(NULL, ds, cl, fps, rms, 3, test_only_at_training_time = FALSE) 

  
  expect_null(test_valid_cross_validator(cv))
  
  
  DECODING_RESULTS <- run_decoding(cv)
  
  zero_one_loss_results <- DECODING_RESULTS[[1]] %>%
    dplyr::filter(train_time == test_time) %>%
    select(zero_one_loss) 
  
  print(Sys.time())
  print(as.vector(t(zero_one_loss_results)))
  
  
  print(object.size(DECODING_RESULTS), units = "Mb")
  
  
  # testing that there are no errors plotting the results
  
  plot(DECODING_RESULTS$rm_main_results)
  plot(DECODING_RESULTS$rm_main_results, result_type = "all")
  plot(DECODING_RESULTS$rm_main_results, plot_type = 'line')

  plot(DECODING_RESULTS$rm_confusion_matrix)
  plot(DECODING_RESULTS$rm_confusion_matrix, result_type = "mutual_information")
  
  
})




test_that("cv_standard can run a decoding analysis using magrittr piping", {
  
  basedir_file_name <- system.file(file.path("extdata", "ZD_500bins_500sampled.Rda"), 
                                   package="NeuroDecodeR")
  
  DECODING_RESULTS <- basedir_file_name %>%
    ds_basic('stimulus_ID', 6, num_label_repeats_per_cv_split = 3) %>%
    cl_max_correlation() %>%
    fp_zscore() %>%
    rm_main_results() %>%
    rm_confusion_matrix() %>%
    cv_standard(num_resample_runs = 3) %>%
    run_decoding()
  
  plot(DECODING_RESULTS$rm_confusion_matrix)
    
  expect_equal(class(DECODING_RESULTS), "list")
  
})

