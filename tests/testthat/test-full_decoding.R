

# devtools::install()

#devtools::load_all()
#rm(list = ls())


basedir_file_name <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NDTr")


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

#rms <- list(rm_main_results(), 
#            rm_confusion_matrix(save_only_same_train_test_time = FALSE))

rms <- list(rm_main_results(aggregate_decision_values = "diag", aggregate_normalized_rank = "diag"), 
            rm_confusion_matrix(save_only_same_train_test_time = FALSE))


#rms <- list(rm_main_results(aggregate_decision_values = FALSE, aggregate_normalized_rank = FALSE), 
#            rm_confusion_matrix(save_only_same_train_test_time = FALSE))


#cv <- cv_standard(ds, cl, fps, rms, 3, test_only_at_training_time = FALSE, run_parallel = FALSE) 

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





