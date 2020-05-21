
# ultimately will use this as the file name in the final version but for now easiest to have absolute path
#basedir_file_name <- '../../data/binned/ZD_150_samples_binned_every_50_samples.Rda'


# devtools::install()


devtools::load_all()
rm(list = ls())

package_base_dir_name <- find.package('NDTr')
basedir_file_name <- paste0(package_base_dir_name, "/data/binned/ZD_150_samples_binned_every_50_samples.Rda")

#basedir_file_name <- '~/research/NDT/NDTr/data/binned/ZD_150_samples_binned_every_50_samples.Rda'

#basedir_file_name <- "/home/faculty/emmCS/research/NDT/other_binned_data/ZD_100_samples_binned_every_30_samples.Rda"



ds <- ds_basic(basedir_file_name, 'stimulus_ID', 6, num_label_repeats_per_cv_split = 3)

# should produce chance results
#ds <- ds_basic(basedir_file_name, 'stimulus_ID', 6, num_label_repeats_per_cv_split = 3,
#               randomly_shuffled_labels_before_running = TRUE)



fps <- list(fp_zscore(), fp_select_k_features(100))
#cl <- cl_svm(kernel = "linear")
#cl <- cl_svm()
cl <- cl_max_correlation()



#ds <- ds_basic(basedir_file_name, 'stimulus_ID', 18, use_count_data = TRUE)
#fps <- list()
#cl <- cl_poisson_naive_bayes()


#rms <- list(rm_main_results(), 
#            rm_confusion_matrix(save_only_same_train_test_time = TRUE))

rms <- list(rm_main_results(), 
            rm_confusion_matrix(save_only_same_train_test_time = FALSE))


cv <- cv_standard(ds, cl, fps, rms, 3, test_only_at_training_time = FALSE) 


#test_valid_cross_validator(cv)


DECODING_RESULTS <- run_decoding(cv)


attributes(DECODING_RESULTS$rm_confusion_matrix)$options


zero_one_loss_results <- DECODING_RESULTS[[1]] %>%
  dplyr::filter(train_time == test_time) %>%
  select(zero_one_loss) 

print(Sys.time())
print(as.vector(t(zero_one_loss_results)))


print(object.size(DECODING_RESULTS), units = "Mb")


plot(DECODING_RESULTS$rm_main_results)
plot(DECODING_RESULTS$rm_confusion_matrix)





