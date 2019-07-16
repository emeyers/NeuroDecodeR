
# ultimately will use this as the file name in the final version but for now easiest to have absolute path
#basedir_file_name <- '../../data/binned/ZD_150_samples_binned_every_50_samples.Rda'


# devtools::install()


devtools::load_all()
rm(list = ls())

basedir_file_name <- '~/research/NDT/NDTr/data/binned/ZD_150_samples_binned_every_50_samples.Rda'

#basedir_file_name <- "/home/faculty/emmCS/research/NDT/other_binned_data/ZD_100_samples_binned_every_30_samples.Rda"



ds <- basic_DS(basedir_file_name, 'stimulus_ID', 18, 0)

fps <- list(fp_zscore())

cl <- cl_max_correlation()

#rms <- list(main_results_RM(), 
#            confusion_matrix_RM(save_only_same_train_test_time = TRUE))

rms <- list(main_results_RM(), 
            confusion_matrix_RM(save_only_same_train_test_time = FALSE))


cv <- standard_CV(ds, cl, fps, 3, rms, test_only_at_training_time = FALSE) 


test_valid_cross_validator(cv)


DECODING_RESULTS <- run_decoding(cv)


attributes(DECODING_RESULTS$confusion_matrix_RM)$options


zero_one_loss_results <- DECODING_RESULTS[[1]] %>%
  dplyr::filter(train_time == test_time) %>%
  select(zero_one_loss) 

print(Sys.time())
print(as.vector(t(zero_one_loss_results)))


print(object.size(DECODING_RESULTS), units = "Mb")


plot(DECODING_RESULTS$main_results_RM)
plot(DECODING_RESULTS$confusion_matrix_RM)





