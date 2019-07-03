


devtools::load_all()
rm(list = ls())

#basedir_file_name <- '~/research/NDT/NDTr/data/binned/ZD_150_samples_binned_every_50_samples.Rda'

basedir_file_name <- "/home/faculty/emmCS/research/NDT/other_binned_data/ZD_100_samples_binned_every_30_samples.Rda"



ds <- basic_DS(basedir_file_name, 'stimulus_ID', 18, 0)
fps <- list(zscore_FP())
cl <- max_correlation_CL()

#rms <- list(main_results_RM(), 
#            confusion_matrix_RM(save_only_same_train_test_time = TRUE))

rms <- list(main_results_RM(), 
            confusion_matrix_RM(save_only_same_train_test_time = TRUE))


cv <- standard_CV(ds, cl, fps, 3, rms) 


library(profvis)
profvis({
  
  DECODING_RESULTS <- run_decoding(cv)

})












