
library(testthat)



real_data_binned_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package = "NeuroDecodeR")



# test the ds_basic -----------------------------------------------------------


test_that("ds_basic() conforms to the NDR datasouce interface", {
  
  ds <- ds_basic(real_data_binned_file_name, 'stimulus_ID', 18, 0)
  expect_null(test_valid_datasource(ds))

})


test_that("ds_basic: the get_data() functions returns unique points in training and test sets (no data leakage)", {
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10)
  the_data <- get_data(ds)
  
  long_data <- the_data %>%
    dplyr::select(starts_with("site")) %>%
    tidyr::gather(site, activity) 
  
  expect_equal(length(unique(long_data$activity)), dim(long_data)[1])
  
})



test_that("ds_basic: if the labels_levels_to_use is set, only those label levels are returned", {
  
  label_levels_to_use <- c("flower", "guitar", "kiwi")
  
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10, label_levels = label_levels_to_use)
  the_data <- get_data(ds)
  
  expect_equal(as.character(unique(the_data$train_labels)), label_levels_to_use)
  expect_equal(as.character(unique(the_data$test_labels)), label_levels_to_use)
  
})




test_that("basic_ds: if only specific sites to be used, only those sites are returned", {
  
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



test_that("basic_ds: the correct number of resampled sites is returned", {
  
  num_resample_sites <- 50
  ds <- ds_basic("fake_binned_data.Rda", "stim_names", 10,  
                 num_resample_sites = num_resample_sites)
  
  the_data <- get_data(ds)
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  
  expect_equal(num_resample_sites, length(site_names))
  
})





test_that("basic_ds: the correct number of repeats per CV block are returned", {
  
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




test_that("basic_ds: simultaneously recorded data is returned correctly", {
  
  
  # all whole numbers on each row should be the same this data when gotten simultaneously 
  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                 create_simultaneous_populations = TRUE)
  
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
  
  
  # all whole numbers on each row should NOT be the same this data when data is NOT gotten simultaneously 
  ds <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                 create_simultaneous_populations = FALSE)
  the_data <- get_data(ds)
  
  the_site_data <- the_data %>%
    select(starts_with('site')) 
  
  expect_false(sum(abs(diff(t(round(data.matrix(the_site_data)))))) == 0)
  
  
  # test that the simultaneous argument works even when trial number is a not variable in 
  # the binned_data data frame
  load("fake_simultaneous_binned_data.Rda")
  binned_data <- binned_data %>%
    dplyr::select(-trial_number)
  
  expect_warning(ds_basic(binned_data, "stim_names", 10,
                          create_simultaneous_populations = TRUE))
  
})




test_that("basic_ds: shuffling labels works", {
  
  
  num_CV <- 5
  num_reps <- 2

  # check 1: when the label levels are not shuffled and we pull all 10 sites, there
  # should always be the same mapping for sites and labels everytime we get a
  # new set of data
  
  ds1 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps)
  
  unshuffled_data1 <- get_data(ds1) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  
  ds2 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps)
  
  unshuffled_data2 <- get_data(ds2) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  expect_true(identical(unshuffled_data1, unshuffled_data2))  
  
  
  
  # check 2a: when the label levels are shuffled before running, and we pull all 10 sites,
  # there be a different mapping for sites and labels everytime we get a new set
  # of data
  
  ds1 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                 num_label_repeats_per_cv_split = num_reps,
                      randomly_shuffled_labels = TRUE)
  
  shuffled_data1 <- get_data(ds1) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  ds2 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps,
                  randomly_shuffled_labels = TRUE)
  
  shuffled_data2 <- get_data(ds2) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  expect_false(identical(shuffled_data1, shuffled_data2))
  
  
  # check 2b: since the shuffling happens before using the get_data() method
  #  a second pull should give rise to the same label site ID shuffling
  
  shuffled_data1_second_pull <- get_data(ds1) %>% 
    select(site_0001, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  expect_true(identical(shuffled_data1, shuffled_data1_second_pull))
  
  
  
  # check 3: when suffling simulatenous data, all sites should have the same
  # random trial to label mapping...
  
  ds_simul1 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps,
                  randomly_shuffled_labels = TRUE,
                  create_simultaneous_populations = TRUE)
  
  simul_shuffled_data1 <- get_data(ds_simul1) %>% 
    mutate(site_0001 = round(site_0001), site_0002 = round(site_0002)) %>%
    select(site_0001, site_0002, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  expect_equal(simul_shuffled_data1$site_0001, simul_shuffled_data1$site_0002) 
  
  
  
  # check 4: when one creates a new data source and get the data a second time using
  # simulatenous populations the mapping should be different from the first
  # time.
  
  
  ds_simul2 <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                        num_label_repeats_per_cv_split = num_reps,
                        randomly_shuffled_labels = TRUE,
                        create_simultaneous_populations = TRUE)
  
  simul_shuffled_data2 <- get_data(ds_simul2) %>% 
    mutate(site_0001 = round(site_0001), site_0002 = round(site_0002)) %>%
    select(site_0001, site_0002, train_labels, test_labels) %>%
    arrange(train_labels, test_labels, site_0001)
  
  
  expect_false(identical(simul_shuffled_data1$site_0001, simul_shuffled_data2$site_0001))
  
  
})





test_that("basic_ds: different cross-validation arrangements of data are returned each time get_data() is called", {
  
  
  num_CV <- 5
  num_reps <- 2
  
  ds_pseudo <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps)
  
  data1 <- get_data(ds_pseudo)
  data2 <- get_data(ds_pseudo)
  
  expect_false(identical(data1, data2))
  
  
  ds_simul <- ds_basic("fake_simultaneous_binned_data.Rda", "stim_names", num_CV,
                  num_label_repeats_per_cv_split = num_reps, 
                  create_simultaneous_populations = TRUE)
  
  
  data1 <- get_data(ds_simul)
  data2 <- get_data(ds_simul)
  
  expect_false(identical(data1, data2))
  
  
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



test_that("generalization_ds constructor and get data work", {
  
  
  ds <- ds_generalization(real_data_binned_file_name, 
                          'combined_ID_position', 18, 
                          train_label_levels, 
                          test_label_levels)
  
  expect_null(test_valid_datasource(ds))
  
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
#  and above chance in stimulus period (more of an integration test than a unit test)

test_that("generalization_ds classification accuracy seem reasonable", {
  
  # get firing rate data
  ds <- ds_generalization(real_data_binned_file_name, 'combined_ID_position',
                 num_cv_splits = 3, num_label_repeats_per_cv_split = 6,
                 train_label_levels = train_label_levels, 
                 test_label_levels = test_label_levels)
  
  cv_data <- get_data(ds)
  
  training_set <- dplyr::filter(cv_data, time_bin == "time.200_350", CV_1 == "train") %>% 
    select(starts_with("site"), train_labels = train_labels)
  
  test_set <- dplyr::filter(cv_data, time_bin %in% c("time.-350_-200", "time.200_350"), CV_1 == "test") %>% 
    select(starts_with("site"), test_labels = test_labels, time_bin)
  
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.-350_-200"] <- "baseline"
  levels(test_set$time_bin)[levels(test_set$time_bin)=="time.200_350"] <- "stimulus"
  
  cl <- cl_max_correlation()
  
  # prediction_results <- get_predictions(cl, normalized_training_set, normalized_test_set)
  prediction_results <- get_predictions(cl, training_set, test_set)
  
  accuracies <- prediction_results %>%
    dplyr::group_by(test_time) %>%
    dplyr::summarize(mean_accuracy = mean(actual_labels == predicted_labels))
  
  expect_gt(dplyr::filter(accuracies, test_time == "stimulus")$mean_accuracy, .49)
  expect_lt(dplyr::filter(accuracies, test_time == "baseline")$mean_accuracy, .3)

})
  


