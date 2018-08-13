binned_file_name <-'C:/Users/14868/Documents/GitHub/NDTr/data/binned/ZD_150_samples_binned_every_10_samples.Rda'
variable_to_decode <-'stimulus_ID'
num_cv_splits <- 5
ds <- NDTr::basic_DS$new(binned_file_name, variable_to_decode, num_cv_splits)
ds$num_repeats_per_level_per_cv_split <- 2
cl <- NDTr::max_correlation_CL$new()
fps <- list()
cv <- NDTr::standard_CV$new(ds, cl, fps)
DECODING_RESULTS <- cv$run_decoding()
save('DECODING_RESULTS', file = 'C:/Users/14868/Documents/GitHub/NDTr/results')
