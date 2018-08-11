binned_data_file_name <-C:/Users/14868/Documents/GitHub/NDTr/data/binned/ZD_150_samples_binned_every_10_samples_start_-300_end_300.Rda
variable_to_decode <-combined_ID_position
num_cv_split <- 5
ds <- basic_DS$new(binned_file_name, specific_binned_label_name, num_cv_splits)
ds$num_repeats_per_level_per_cv_split <- 2
cl <- maximum_correlation_CL$new()
fps <-list ()
cv <- standard_CV$new(ds, cl, fps)
DECODING_RESULTS <- cv$run_decoding()
