
# This file contains code that generates data useful for testing the NDR



 # data useful for testing datasources ------------------------------

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







# data useful for testing classifiers and feature proprocessors ------------

real_data_binned_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package="NeuroDecodeR")


# get firing rate data (just use data at 200-350 post stimulus onset for testing)
ds <- ds_basic(real_data_binned_file_name, "stimulus_ID", site_IDs_to_exclude = 63,
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
cv_data <- get_data(ds)
training_set <- filter(cv_data, time_bin == "time.200_350", CV_1 == "train") %>% select(starts_with("site"), train_labels)
test_set <- filter(cv_data, time_bin %in% c("time.-350_-200", "time.200_350"), CV_1 == "test") %>% select(starts_with("site"), test_labels, time_bin)
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-200"] <- "baseline"
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_350"] <- "stimulus"



# get data a spike counts
ds <- ds_basic(real_data_binned_file_name, "stimulus_ID", site_IDs_to_exclude = 63,
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, use_count_data = TRUE)
cv_data <- get_data(ds)
count_training_set <- filter(cv_data, time_bin == "time.200_350", CV_1 == "train") %>% select(starts_with("site"), train_labels)
count_test_set <- filter(cv_data, time_bin %in% c("time.-350_-200", "time.200_350"), CV_1 == "test") %>% select(starts_with("site"), test_labels, time_bin)
levels(count_test_set$time_bin)[levels(count_test_set$time_bin)=="time.-350_-200"] <- "baseline"
levels(count_test_set$time_bin)[levels(count_test_set$time_bin)=="time.200_350"] <- "stimulus"


# get data z-score normalized data
fp <- fp_zscore()
processed_data <- preprocess_data(fp, training_set, test_set)
normalized_training_set <- processed_data$training_set
normalized_test_set <- processed_data$test_set


# generate shuffled data...
ds <- ds_basic(real_data_binned_file_name, "stimulus_ID", site_IDs_to_exclude = 63,
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, randomly_shuffled_labels = TRUE)
cv_data <- get_data(ds)
training_set <- filter(cv_data, time_bin == "time.200_350", CV_1 == "train") %>% select(starts_with("site"), train_labels)
test_set <- filter(cv_data, time_bin %in% c("time.-350_-200", "time.200_350"), CV_1 == "test") %>% 
  select(starts_with("site"), test_labels, time_bin)
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-200"] <- "baseline"
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_350"] <- "stimulus"
fp <- fp_zscore()
processed_data <- preprocess_data(fp, training_set, test_set)
shuffled_normalized_training_set <- processed_data$training_set
shuffled_normalized_test_set <- processed_data$test_set



# generate shuffled count data...
ds <- ds_basic(real_data_binned_file_name, "stimulus_ID", site_IDs_to_exclude = 63,
               num_cv_splits = 3, num_label_repeats_per_cv_split = 6, 
               randomly_shuffled_labels = TRUE, 
               use_count_data = TRUE)
cv_data <- get_data(ds)
training_set <- filter(cv_data, time_bin == "time.200_350", CV_1 == "train") %>% select(starts_with("site"), train_labels)
test_set <- filter(cv_data, time_bin %in% c("time.-350_-200", "time.200_350"), CV_1 == "test") %>% 
  select(starts_with("site"), test_labels, time_bin)
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-200"] <- "baseline"
levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_350"] <- "stimulus"
shuffled_count_training_set <- training_set
shuffled_count_test_set <- test_set


save(training_set, test_set,
     normalized_training_set, normalized_test_set,
     count_training_set, count_test_set,
     shuffled_normalized_training_set, shuffled_normalized_test_set,
     shuffled_count_training_set,
     shuffled_count_test_set,
     file = "example_ZD_train_and_test_set.Rda")






# data for testing saving the results ------------------


# generate data to get saving the results, this could will take a while to run...
real_data_binned_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package="NeuroDecodeR")


# no z-score preprocessor
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list()
cl <- cl_max_correlation()
rms <- list(rm_main_results(),
            rm_confusion_matrix())
cv <- cv_standard(NULL, ds, cl, fps, rms, 2)
DECODING_RESULTS_1 <- run_decoding(cv)


# no rm_confusion_matrix
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(fp_zscore())
cl <- cl_max_correlation()
rms <- list(rm_main_results())
cv <- cv_standard(NULL, ds, cl, fps, rms, 2)
DECODING_RESULTS_2 <- run_decoding(cv)


# standard results
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(fp_zscore())
cl <- cl_max_correlation()
rms <- list(rm_main_results(),
            rm_confusion_matrix())
cv <- cv_standard(NULL, ds, cl, fps, rms, 2)
DECODING_RESULTS_3 <- run_decoding(cv)


# use only neurons 1 to 100
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0, site_IDs_to_use = 1:100)
fps <- list(fp_zscore())
cl <- cl_max_correlation()
rms <- list(rm_main_results(),
            rm_confusion_matrix())
cv <- cv_standard(NULL, ds, cl, fps, rms, 2)
DECODING_RESULTS_4 <- run_decoding(cv)


# standard results again
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
fps <- list(fp_zscore())
cl <- cl_max_correlation()
rms <- list(rm_main_results(),
            rm_confusion_matrix())
cv <- cv_standard(NULL, ds, cl, fps, rms, 2)
DECODING_RESULTS_5 <- run_decoding(cv)


save(DECODING_RESULTS_1, 
     DECODING_RESULTS_2, 
     DECODING_RESULTS_3, 
     DECODING_RESULTS_4, 
     DECODING_RESULTS_5, 
     file = "example_DECODING_RESULTS.Rda")



# data for running the full decoding ------------------


# raster_dir_name <- trimws(file.path(system.file("extdata", package = "NeuroDecodeR"), 
#                                     "Zhang_Desimone_7object_raster_data_rda", " "))
# 
# binned_file_name <- create_binned_data(raster_dir_name, "ZD", 500, 500)

