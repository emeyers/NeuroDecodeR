

run_test_full_decoding <- function(cv, plot_results = FALSE) {
  
  DECODING_RESULTS <- cv$run_decoding()
  
  if (plot_results) {
    plot(DECODING_RESULTS$zero_one_loss_results)
    temp_mean_results <- colMeans(DECODING_RESULTS$zero_one_loss_results)
    plot(diag(temp_mean_results), type = 'o')
  }

  return(DECODING_RESULTS)
  
}



basedir_file_name <- '../../data/binned/ZD_150_samples_binned_every_50_samples.Rda'
ds <- NDTr::basic_DS$new(basedir_file_name, 'stimulus_ID', 18, 0)
fps <- list(NDTr::zscore_FP$new())
cl <- NDTr::max_correlation_CL$new()
cv <- NDTr::standard_CV$new(ds, cl, fps) 

DECODING_RESULTS <- run_test_full_decoding(cv, TRUE)



# test if shuffling the results returns results at chance...
# ds <- NDTr::basic_DS$new(basedir_file_name, 'stimulus_ID', 18, 0, randomly_shuffled_labels_before_running = TRUE)
# cv <- NDTr::standard_CV$new(ds, cl, fps) 
# DECODING_RESULTS_SHUFFLED <- run_test_full_decoding(cv)












