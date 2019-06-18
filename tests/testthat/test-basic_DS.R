
library(testthat)




real_data_binned_file_name <- file.path("..", "..", "data", "binned", "ZD_150_samples_binned_every_50_samples.Rda")



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


binned_data <- data.frame(siteID = rep(1:100, each = 70),
                          trial_number = rep(1:70, 100),
                          labels.stim_names = rep(c("car", "couch", "face", "flower", "guitar", "hand", "kiwi"), 1000),
                          time.1_100 = as.double(paste0(rep(1:70, 100), '.', sprintf("%04d", rep(1:100, each = 70)))),
                          time.101_200 = as.double(paste0(rep(71:140, 100), '.', sprintf("%04d", rep(1:100, each = 70)))), 
                          time.201_300 = as.double(paste0(rep(141:210, 100), '.', sprintf("%04d", rep(1:100, each = 70)))), 
                          time.301_400 = as.double(paste0(rep(211:280, 100), '.', sprintf("%04d", rep(1:100, each = 70)))))

# save(binned_data, file = "fake_simultaneous_binned_data.Rda")






test_that("basic_DS$get_data() returns unique points in training and test sets (no data leakage)", {
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10)
  the_data <- ds$get_data()
  
  long_data <- the_data %>%
    dplyr::select(starts_with("site")) %>%
    tidyr::gather(site, activity) 
  
  expect_equal(length(unique(long_data$activity)), dim(long_data)[1])
  
})







test_that("if labels_levels_to_use is set, only those label levels are returned", {

  labels_levels_to_use <- c("flower", "guitar", "kiwi")
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10, label_levels_to_use = labels_levels_to_use)

  the_data <- ds$get_data()
  expect_equal(as.character(unique(the_data$labels)), labels_levels_to_use)

})






test_that("if only specific sites to be used, only those sites are returned", {
  
  # use only a subset of sites
  sites_to_use <- sites_to_use <- seq(1, 100, 3)
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10, site_IDs_to_use = sites_to_use)
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(sites_to_use, site_numbers)
  
  
  # exclude a subset of sites
  sites_to_exclude <- seq(2, 100, 5)
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10,  site_IDs_to_exclude = sites_to_exclude)
  remaining_sites <- setdiff(1:100, sites_to_exclude)
  
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites, site_numbers)
  
  
  # use and exclude a subset of sites
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10,   
                     site_IDs_to_use = sites_to_use, site_IDs_to_exclude = sites_to_exclude)

  remaining_sites_used <- setdiff(sites_to_use, sites_to_exclude)
  
  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  site_numbers <- as.numeric(stringr::str_sub(site_names, 6, 9))
  
  expect_equal(remaining_sites_used, site_numbers)
  
  
})




test_that("the correct number of resampled sites is returned", {

  num_resample_sites <- 50
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", 10,  
                     num_resample_sites = num_resample_sites)

  the_data <- ds$get_data()
  site_names <- the_data %>% 
    dplyr::select(starts_with("site")) %>% 
    names(.)
  
  expect_equal(num_resample_sites, length(site_names))
  
})





test_that("the correct number of repeats per CV block are returned", {
  
  num_CV <- 5
  num_reps <- 2
  
  ds <- basic_DS$new("fake_binned_data.Rda", "stim_names", num_CV,
                     num_label_repeats_per_cv_split = num_reps)
                     
  the_data <- ds$get_data()
  
  total_num_labels <- the_data %>%
    group_by(time, labels) %>%
    summarize(n = n()) 
  
  expect_equal(total_num_labels$n, rep(num_CV * num_reps, 7 * length(unique(the_data$time))))
  
})




# test create simultaneously recorded data is working
test_that("simultaneously recorded data is returned correctly", {
 
  
  # all whole numbers on each row should be the same this data when gotten simultaneously 
  ds <- basic_DS$new("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                     create_simultaneously_recorded_populations = TRUE)

  the_data <- ds$get_data()
  
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
  ds <- basic_DS$new("fake_simultaneous_binned_data.Rda", "stim_names", 10,
                     create_simultaneously_recorded_populations = FALSE)
  the_data <- ds$get_data()
  
  the_site_data <- the_data %>%
    select(starts_with('site')) 
  
  expect_false(sum(abs(diff(t(round(data.matrix(the_site_data)))))) == 0)
  
  
})









