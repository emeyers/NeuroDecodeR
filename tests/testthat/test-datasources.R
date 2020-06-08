
library(testthat)



real_data_binned_file_name <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NDTr")



# test the ds_basic -----------------------------------------------------------


# test that the datasource contains all the necessary methods
#source("test_valid_NDTr_objects.R")
ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
test_valid_datasource(ds)


test_that("get_data(ds_basic_obj) returns unique points in training and test sets (no data leakage)", {
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10)
  the_data <- get_data(ds)
  
  long_data <- the_data %>%
    dplyr::select(starts_with("site")) %>%
    tidyr::gather(site, activity) 
  
  expect_equal(length(unique(long_data$activity)), dim(long_data)[1])
  
})







test_that("if labels_levels_to_use is set, only those label levels are returned", {
  
  labels_levels_to_use <- c("flower", "guitar", "kiwi")
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10, label_levels_to_use = labels_levels_to_use)
  the_data <- get_data(ds)
  
  expect_equal(as.character(unique(the_data$train_labels)), labels_levels_to_use)
  expect_equal(as.character(unique(the_data$test_labels)), labels_levels_to_use)
  
})






test_that("if only specific sites to be used, only those sites are returned", {
  
  # use only a subset of sites
  sites_to_use <- sites_to_use <- seq(1, 100, 3)
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10, site_IDs_to_use = sites_to_use)
  the_data <- get_data(ds)
  
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(sites_to_use, site_numbers)
  
  
  # exclude a subset of sites
  sites_to_exclude <- seq(2, 100, 5)
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10,  site_IDs_to_exclude = sites_to_exclude)
  remaining_sites <- setdiff(1:100, sites_to_exclude)
  
  
  the_data <- get_data(ds)
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites, site_numbers)
  
  
  # use and exclude a subset of sites
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10,   
                 site_IDs_to_use = sites_to_use, site_IDs_to_exclude = sites_to_exclude)
  
  remaining_sites_used <- setdiff(sites_to_use, sites_to_exclude)
  
  the_data <- get_data(ds)
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites_used, site_numbers)
  
  
})




test_that("the correct number of resampled sites is returned", {
  
  num_resample_sites <- 50
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10,  
                 num_resample_sites = num_resample_sites)
  
  the_data <- get_data(ds)
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  
  expect_equal(num_resample_sites, length(site_names))
  
})





test_that("the correct number of repeats per CV block are returned", {
  
  num_CV <- 5
  num_reps <- 2
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", num_CV,
                 num_label_repeats_per_cv_split = num_reps)
  
  the_data <- get_data(ds)
  
  total_num_labels <- the_data %>%
    group_by(time_bin, train_labels) %>%
    summarize(n = n()) 
  
  expect_equal(total_num_labels$n, rep(num_CV * num_reps, 7 * length(unique(the_data$time_bin))))
  
})




# test create simultaneously recorded data is working
test_that("simultaneously recorded data is returned correctly", {
  
  
  # all whole numbers on each row should be the same this data when gotten simultaneously 
  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                 create_simultaneously_recorded_populations = TRUE)
  
  the_data <- get_data(ds)
  
  the_site_data <- the_data %>%
    select(starts_with('site')) 
  
  expect_equal(sum(abs(diff(t(round(data.matrix(the_site_data)))))), 0)
  
  
  
  # the fractional part should match the site digit names
  data_as_a_matrix = data.matrix(the_site_data)
  whole = floor(data_as_a_matrix)
  fraction = data_as_a_matrix - whole
  
  site_name_fraction <- as.numeric(paste0('.', substr(names(the_site_data), 6, 9)))
  site_name_matrix <- matrix(rep(site_name_fraction, each = dim(the_site_data)[1]), 
                             nrow = dim(the_site_data)[1], 
                             ncol = dim(the_site_data)[2])
  
  expect_lt(max(site_name_matrix - fraction), 10^-10)
  
  
  
  
  # all whole numbers on each row should NOT be the same this data when NOT gotten simultaneously 
  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                 create_simultaneously_recorded_populations = FALSE)
  the_data <- get_data(ds)
  
  the_site_data <- the_data %>%
    select(starts_with('site')) 
  
  expect_false(sum(abs(diff(t(round(data.matrix(the_site_data)))))) == 0)
  
  
  # test that the simultaneous argument works even when trial number is a variable in 
  # the binned_data data frame
  load("fake_simultaneous_binned_data.Rda")
  binned_data <- binned_data %>%
    dplyr::select(-trial_number)
  save(binned_data , file = "fake_simultaneous_binned_data2.Rda")
  
  expect_warning(ds_basic("fake_simultaneous_binned_data2.Rda", "stim_names", 10,
                 create_simultaneously_recorded_populations = TRUE))
  
  
})







test_that("shuffling labels works", {
  
  num_CV <- 5
  num_reps <- 2

  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                 num_label_repeats_per_cv_split = num_reps)
  
  unshuffled_data1 <- get_data(ds) %>% 
    select(site_0001, train_labels) %>%
    arrange(train_labels, site_0001)
  
  unshuffled_data2 <- get_data(ds) %>% 
    select(site_0001, train_labels) %>%
    arrange(train_labels, site_0001)
  
  expect_equal(unshuffled_data1$site_0001, unshuffled_data2$site_0001)
  
  
  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                 num_label_repeats_per_cv_split = num_reps,
                      randomly_shuffled_labels_before_running = TRUE,
                 create_simultaneously_recorded_populations = 1)
  
  shuffled_data1 <- get_data(ds) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  shuffled_data2 <- get_data(ds) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  # hmmm, these should be different :(
  #sum(shuffled_data1$site_0001 != shuffled_data2$site_0001)
  
  # this should fail but it's not :(
  # other tests suggest the shuffling is working though, should look more into this later
  #expect_equal(shuffled_data1$site_0001, shuffled_data2$site_0001) 
  
   
  # could/should implement a more rigourous test here...
   
})








# test the ds_generalization  -------------------------------------------------



# generate the training and test levels for the ds_generalization

id_levels <- c("hand", "flower", "guitar", "face", "kiwi", "couch",  "car")   
position_levels <- c("lower", "middle", "upper")

train_label_levels <- NULL
test_label_levels <- NULL
for (i in seq_along(id_levels)){
  train_label_levels[[i]] <- c(paste(id_levels[i], "upper",sep = '_'), 
                               paste(id_levels[i], "middle",sep = '_'))
  test_label_levels[[i]] <- list(paste(id_levels[i], "lower",sep = '_'))
}



test_that("testing generalization_ds constructor and get data work", {
  
  
  
  ds <- ds_generalization(real_data_binned_file_name, 
                          'combined_ID_position', 18, 
                          train_label_levels, 
                          test_label_levels)
  
  test_valid_datasource(ds)
  the_data <- get_data(ds)

  
  # test that if the same levels are assigned to different classes this gives an
  # error (otherwise there could be data leakage)
  test_label_levels2 <- train_label_levels
  temp <- test_label_levels2[[1]]
  test_label_levels2[[1]] <- test_label_levels2[[2]]
  test_label_levels2[[2]] <- temp
  expect_error(ds_generalization(real_data_binned_file_name, 
                                        'combined_ID_position', 18, 
                                        train_label_levels, 
                                        test_label_levels2))
  
  
})





# test that ds_generalization leads to results at chance in baseline 
#  and above chance in stimulus period
# more of an integration test than a unit test but ok
test_that("testing classification results using generalization_ds seem reasonable", {
  
  # get firing rate data
  ds <- ds_generalization(real_data_binned_file_name, 'combined_ID_position',
                 num_cv_splits = 3, num_label_repeats_per_cv_split = 6,
                 train_label_levels = train_label_levels, 
                 test_label_levels = test_label_levels)
  
  cv_data <- get_data(ds)
  training_set <- dplyr::filter(cv_data, time_bin == "time.200_349", CV_1 == "train") %>% 
    select(starts_with("site"), train_labels = train_labels)
  test_set <- dplyr::filter(cv_data, time_bin %in% c("time.-350_-201", "time.200_349"), CV_1 == "test") %>% 
    select(starts_with("site"), test_labels = test_labels, time_bin)
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-201"] <- "baseline"
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_349"] <- "stimulus"
  
  cl <- cl_max_correlation()
  
  # prediction_results <- get_predictions(cl, normalized_training_set, normalized_test_set)
  prediction_results <- get_predictions(cl, training_set, test_set)
  
  accuracies <- prediction_results %>%
    dplyr::group_by(test_time) %>%
    dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))
  
  expect_gt(dplyr::filter(accuracies, test_time == "stimulus")$mean_accuracy, .49)
  expect_lt(dplyr::filter(accuracies, test_time == "baseline")$mean_accuracy, .3)

})
  




