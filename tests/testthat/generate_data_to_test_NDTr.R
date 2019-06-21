
# This file contains code that generates data useful for testing the NDTr



#  # data useful for testing data sources
#
#  # Here each time point has a unique integer
# binned_data <- data.frame(siteID = rep(1:100, each = 70), 
#                           labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000), 
#                           time.1_100 = 1:7000, 
#                           time.101_200 = 7001:14000, 
#                           time.201_300 = 14001:21000,
#                           time.301_400 = 21001:28000)
# 
# # save(binned_data, file = "fake_binned_data.Rda")
# 
#
# 
#  # Here each whole value corresponds to a trial number and each decimal corresponds to a siteID
# binned_data <- data.frame(siteID = rep(1:100, each = 70),
#                           trial_number = rep(1:70, 100),
#                           labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000),
#                           time.1_100 = as.double(paste0(rep(1:70, 100), '.', sprintf("%04d", rep(1:100, each = 70)))),
#                           time.101_200 = as.double(paste0(rep(71:140, 100), '.', sprintf("%04d", rep(1:100, each = 70)))), 
#                           time.201_300 = as.double(paste0(rep(141:210, 100), '.', sprintf("%04d", rep(1:100, each = 70)))), 
#                           time.301_400 = as.double(paste0(rep(211:280, 100), '.', sprintf("%04d", rep(1:100, each = 70)))))
# 
# # save(binned_data, file = "fake_simultaneous_binned_data.Rda")
# 




# data useful for testing classifiers and feature proprocessors

# real_data_binned_file_name <- file.path("..", "..", "data", "binned", "ZD_150_samples_binned_every_50_samples.Rda")
# 
#  # just use data at 200-349 post stimulus onset for testing
# 
# 
# # get firing rate data
# ds <- basic_DS(real_data_binned_file_name, "stimulus_ID",
#                num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
# cv_data <- get_data(ds)
# train_set <- filter(cv_data, time == "time.200_349", CV_1 == "train")
# test_set <- filter(cv_data, time == "time.200_349", CV_1 == "test")
# 
# 
# # get data a spike counts
# ds <- basic_DS(real_data_binned_file_name, "stimulus_ID",
#                num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
# cv_data <- get_data(ds)
# count_train_set <- filter(cv_data, time == "time.200_349", CV_1 == "train")
# count_test_set <- filter(cv_data, time == "time.200_349", CV_1 == "test")
# 
# 
# # get data z-score normalized
# fp <- zscore_FP()
# processed_data <- preprocess_data(fp, train_set, test_set)
# normalized_train_set <- processed_data$train_set
# normalized_test_set <- processed_data$test_set
# 
# 
# save(train_set, test_set,
#      normalized_train_set, normalized_test_set,
#      count_train_set, count_test_set,
#      file = "example_ZD_train_and_test_set.Rda")


