
# This file contains code that generates data useful for testing the NDTr



 # data useful for testing data sources

 # Here each time point has a unique integer
binned_data <- data.frame(siteID = rep(1:100, each = 70),
                          labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000),
                          time.1_100 = 1:7000,
                          time.101_200 = 7001:14000,
                          time.201_300 = 14001:21000,
                          time.301_400 = 21001:28000)

save(binned_data, file = "fake_binned_data.Rda")



 # Here each whole value corresponds to a trial number and each decimal corresponds to a siteID
binned_data <- data.frame(siteID = rep(1:100, each = 70),
                          trial_number = rep(1:70, 100),
                          labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000),
                          time.1_100 = as.double(paste0(rep(1:70, 100), '.', sprintf("%04d", rep(1:100, each = 70)))),
                          time.101_200 = as.double(paste0(rep(71:140, 100), '.', sprintf("%04d", rep(1:100, each = 70)))),
                          time.201_300 = as.double(paste0(rep(141:210, 100), '.', sprintf("%04d", rep(1:100, each = 70)))),
                          time.301_400 = as.double(paste0(rep(211:280, 100), '.', sprintf("%04d", rep(1:100, each = 70)))))

save(binned_data, file = "fake_simultaneous_binned_data.Rda")





# data useful for testing classifiers and feature proprocessors

real_data_binned_file_name <- file.path("..", "..", "data", "binned", "ZD_150_samples_binned_every_50_samples.Rda")

 # just use data at 200-349 post stimulus onset for testing


# get firing rate data
ds <- basic_DS(real_data_binned_file_name, "stimulus_ID",
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
cv_data <- get_data(ds)
training_set <- filter(cv_data, time_bin == "time.200_349", CV_1 == "train") %>% select(starts_with("site"), labels)
test_set <- filter(cv_data, time_bin %in% c("time.-350_-201", "time.200_349"), CV_1 == "test") %>% select(starts_with("site"), labels, time_bin)
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-201"] <- "baseline"
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_349"] <- "stimulus"

# get data a spike counts
ds <- basic_DS(real_data_binned_file_name, "stimulus_ID",
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
cv_data <- get_data(ds)
count_training_set <- filter(cv_data, time_bin == "time.200_349", CV_1 == "train") %>% select(starts_with("site"), labels)
count_test_set <- filter(cv_data, time_bin %in% c("time.-350_-201", "time.200_349"), CV_1 == "test") %>% select(starts_with("site"), labels, time_bin)
levels(count_test_set$time_bin)[levels(count_test_set$time_bin)=="time.-350_-201"] <- "baseline"
levels(count_test_set$time_bin)[levels(count_test_set$time_bin)=="time.200_349"] <- "stimulus"


# get data z-score normalized
fp <- zscore_FP()
processed_data <- preprocess_data(fp, training_set, test_set)
normalized_training_set <- processed_data$training_set
normalized_test_set <- processed_data$test_set


# generate shuffled data...
ds <- basic_DS(real_data_binned_file_name, "stimulus_ID",
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, randomly_shuffled_labels_before_running = TRUE)
cv_data <- get_data(ds)
training_set <- filter(cv_data, time_bin == "time.200_349", CV_1 == "train") %>% select(starts_with("site"), labels)
test_set <- filter(cv_data, time_bin %in% c("time.-350_-201", "time.200_349"), CV_1 == "test") %>% 
  select(starts_with("site"), labels, time_bin)
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-201"] <- "baseline"
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_349"] <- "stimulus"
fp <- zscore_FP()
processed_data <- preprocess_data(fp, training_set, test_set)
shuffled_normalized_training_set <- processed_data$training_set
shuffled_normalized_test_set <- processed_data$test_set



save(training_set, test_set,
     normalized_training_set, normalized_test_set,
     count_training_set, count_test_set,
     shuffled_normalized_training_set, shuffled_normalized_test_set,
     file = "example_ZD_train_and_test_set.Rda")




# generate data to get saving the results, this could will take a while to run...
real_data_binned_file_name <- file.path("..", "..", "data", "binned", "ZD_150_samples_binned_every_50_samples.Rda")


# no z-score preprocessor
ds <- basic_DS(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list()
cl <- max_correlation_CL()
rms <- list(main_results_RM(),
            confusion_matrix_RM())
cv <- standard_CV(ds, cl, fps, 3, rms)
DECODING_RESULTS_1 <- run_decoding(cv)


# no confusion_matrix_RM
ds <- basic_DS(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(zscore_FP())
cl <- max_correlation_CL()
rms <- list(main_results_RM())
cv <- standard_CV(ds, cl, fps, 3, rms)
DECODING_RESULTS_2 <- run_decoding(cv)


# standard results
ds <- basic_DS(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(zscore_FP())
cl <- max_correlation_CL()
rms <- list(main_results_RM(),
            confusion_matrix_RM())
cv <- standard_CV(ds, cl, fps, 3, rms)
DECODING_RESULTS_3 <- run_decoding(cv)


# use only neurons 1 to 100
ds <- basic_DS(real_data_binned_file_name, 'stimulus_ID', 18, 0, site_IDs_to_use = 1:100)
fps <- list(zscore_FP())
cl <- max_correlation_CL()
rms <- list(main_results_RM(),
            confusion_matrix_RM())
cv <- standard_CV(ds, cl, fps, 3, rms)
DECODING_RESULTS_4 <- run_decoding(cv)


# standard results again
ds <- basic_DS(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(zscore_FP())
cl <- max_correlation_CL()
rms <- list(main_results_RM(),
            confusion_matrix_RM())
cv <- standard_CV(ds, cl, fps, 3, rms)
DECODING_RESULTS_5 <- run_decoding(cv)


save(DECODING_RESULTS_1, 
     DECODING_RESULTS_2, 
     DECODING_RESULTS_3, 
     DECODING_RESULTS_4, 
     DECODING_RESULTS_5, 
     file = "example_DECODING_RESULTS.Rda")













