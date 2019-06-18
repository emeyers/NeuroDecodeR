
library(testthat)




# ds <- basic_DS$new(file.path("..", "..", "data", "binned", "ZD_150_samples_binned_every_50_samples.Rda"), "stimulus_ID", 10)


# test creating a basic DS
#load_all()


binned_data <- data.frame(siteID = rep(1:100, each = 70), 
                          labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000), 
                          time.1_100 = 1:7000, 
                          time.101_200 = 7001:14000, 
                          time.201_300 = 14001:21000,
                          time.301_400 = 21001:28000)

# save(binned_data, file = "fake_binned_data.Rda")




test_that("basic_DS$get_data() returns unique points in training and test sets (no data leakage)", {
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  the_data <- ds$get_data()
  
  long_data <- the_data %>%
    dplyr::select(starts_with("site")) %>%
    tidyr::gather(site, activity) 
  
  expect_equal(length(unique(long_data$activity)), dim(long_data)[1])
  
})






test_that("if labels_levels_to_use is set, only those label levels are returned", {

  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  labels_levels_to_use <- c("flower", "guitar", "kiwi")
  ds$label_levels_to_use <- labels_levels_to_use 
  
  the_data <- ds$get_data()
  expect_equal(as.character(unique(the_data$labels)), labels_levels_to_use)

})






test_that("if only specific sites to be used, only those sites are returned", {
  
  # use only a subset of sites
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  sites_to_use <- sites_to_use <- seq(1, 100, 3)
  ds$site_IDs_to_use <- sites_to_use
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(sites_to_use, site_numbers)
  
  
  # exclude a subset of sites
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  sites_to_exclude <- seq(2, 100, 5)
  remaining_sites <- setdiff(1:100, sites_to_exclude)
  ds$site_IDs_to_exclude <- sites_to_exclude
  
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites, site_numbers)
  
  
  # use and exclude a subset of sites
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  ds$site_IDs_to_use <- sites_to_use
  ds$site_IDs_to_exclude <- sites_to_exclude
  
  remaining_sites_used <- setdiff(sites_to_use, sites_to_exclude)
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites_used, site_numbers)
  
  
})




test_that("the correct number of resampled sites is returned", {
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  num_resample_sites <- 50
  ds$num_resample_sites <- num_resample_sites
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  
  expect_equal(num_resample_sites, length(site_names))
  
})





test_that("the correct number of repeats per CV block are returned", {
  
  num_CV <- 5
  num_reps <- 2
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", num_CV)
  ds$num_label_repeats_per_cv_split <- num_reps
  
  the_data <- ds$get_data()
  
  total_num_labels <- the_data %>%
    group_by(time, labels) %>%
    summarize(n = n()) 
  
  expect_equal(total_num_labels$n, rep(num_CV * num_reps, 7 * length(unique(the_data$time))))
  
})




# test randomly shuffled before running is working





# test create simultaneous recorded data is working













