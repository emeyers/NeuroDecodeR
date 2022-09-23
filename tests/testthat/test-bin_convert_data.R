


# raster_dir_name <- trimws(file.path(system.file("extdata", package = "NeuroDecodeR"), 
#                             "Zhang_Desimone_7object_raster_data_rda", " "))

raster_dir_name <- trimws(file.path(system.file("extdata", package = "NeuroDecodeR"), 
                                    "Zhang_Desimone_7object_raster_data_small_rda", " "))

csv_raster_dir_name <- trimws(file.path(system.file("extdata", package = "NeuroDecodeR"), 
                                    "Zhang_Desimone_7object_raster_data_small_csv", " "))


temp_dir_name <- tempdir()



# testing reading raster_data format data from different raster data types  ---------------

test_that("read_raster_data() can read csv data into raster format", {
  
  csv_raster_file_name <- file.path(
     system.file("extdata", package = "NeuroDecodeR"),
     "Zhang_Desimone_7object_raster_data_small_csv",
     "bp1001spk_01A_raster_data.csv")
  
  # read the csv file into a raster_data data frame
  raster_data <- read_raster_data(csv_raster_file_name)
  
  expect_equal(class(raster_data)[1], "raster_data")
  
})



test_that("read_matlab_raster_data() can read matlab data into raster format", {
  
  matlab_raster_file_name <- file.path(
     system.file("extdata", package = "NeuroDecodeR"),
     "Zhang_Desimone_7object_raster_data_small_mat",
     "bp1001spk_01A_raster_data.mat")
  
  # read the csv file into a raster_data data frame
  raster_data <- read_matlab_raster_data(matlab_raster_file_name)
  
  expect_equal(class(raster_data)[1], "raster_data")
  
})




test_that("read_raster_data() can read rda data into raster format", {
  
  rda_raster_file_name <- file.path(
    system.file("extdata", package = "NeuroDecodeR"),
    "Zhang_Desimone_7object_raster_data_small_rda",
    "bp1001spk_01A_raster_data.rda")
  
  # read the csv file into a raster_data data frame
  raster_data <- read_raster_data(rda_raster_file_name)
  
  expect_equal(class(raster_data)[1], "raster_data")
  
})



test_that("read_raster_data() can read rds data into raster format", {
  
  rds_raster_file_name <- file.path(
    system.file("extdata", package = "NeuroDecodeR"),
    "Zhang_Desimone_7object_raster_data_small_rds",
    "bp1001spk_01A_raster_data.rds")
  
  # read the csv file into a raster_data data frame
  raster_data <- read_raster_data(rds_raster_file_name)
  
  expect_equal(class(raster_data)[1], "raster_data")
  
})





# testing create_binned_data --------------------------------------------------

test_that("create_binned_data() creates binned data from raster data in the correct format", {

  name_of_file_that_should_be_created <- file.path(temp_dir_name, "ZD_150bins_50sampled.Rda") 

  # deleting "ZD_150_samples_binned_every_50_samples.Rda" if it already exist
  if (file.exists(name_of_file_that_should_be_created)) {
    file.remove(name_of_file_that_should_be_created)
  } 
  
  save_prefix <- file.path(temp_dir_name, "ZD")
  
  binned_file_name <- create_binned_data(raster_dir_name, save_prefix, 
                                         150, 50, files_contain = "bp1001spk",
                                         num_parallel_cores = 2)

  expect_equal(name_of_file_that_should_be_created, binned_file_name)

  test_valid_binned_format(binned_file_name)

  file.remove(name_of_file_that_should_be_created)

})



test_that("create_binned_data() creates binned data from csv raster data in the correct format", {
  
  name_of_file_that_should_be_created <- file.path(temp_dir_name, "csv_ZD_150bins_50sampled.Rda") 
  
  # deleting "csv_ZD_150_samples_binned_every_50_samples.Rda" if it already exist
  if (file.exists(name_of_file_that_should_be_created)) {
    file.remove(name_of_file_that_should_be_created)
  } 
  
  save_prefix <- file.path(temp_dir_name, "csv_ZD")
  
  binned_file_name <- create_binned_data(csv_raster_dir_name, save_prefix, 
                                         150, 50, files_contain = "bp1001spk",
                                         num_parallel_cores = 2)
  
  expect_equal(name_of_file_that_should_be_created, binned_file_name)
  
  test_valid_binned_format(binned_file_name)
  
  file.remove(name_of_file_that_should_be_created)
  
})







# testing converting MATLAB raster data to R raster data ----------------------

test_that("convert_matlab_raster_data() convert MATLAB raster data to R raster data", {
  

  matlab_raster_dir_name <- file.path(system.file("extdata", package = "NeuroDecodeR"), 
                                   "Zhang_Desimone_7object_raster_data_small_mat")
  
  # create temporary directory to hold converted data
  r_raster_dir_name <- trimws(file.path(temp_dir_name, " "))
  
  
  # delete any saved results and manifest files that already exist
  if (file.exists(file.path(dirname(r_raster_dir_name), basename(r_raster_dir_name)))) {
    the_files <- paste0(r_raster_dir_name, list.files(r_raster_dir_name))
    file.remove(the_files)
    unlink(file.path(dirname(r_raster_dir_name), basename(r_raster_dir_name)), recursive = TRUE, force = TRUE)
  } 
  
  
  dir.create(r_raster_dir_name)

  
  r_raster_dir_name <- convert_matlab_raster_data(matlab_raster_dir_name, 
                                                r_raster_dir_name, 
                                                files_contain = "bp1001spk")

  # r_raster_dir_name <- convert_matlab_raster_data(matlab_raster_dir_name,
  #                                                start_ind = 300,
  #                                                start_ind = 800,
  #                                                files_contain = "bp1001spk")



  rda_converted_file <- paste0(r_raster_dir_name, list.files(r_raster_dir_name)[1])
  
  # will return NULL if the converted data is correctly in raster format
  expect_null(test_valid_raster_format(rda_converted_file))
  
  
  
  # test saving converting files to csv 
  convert_matlab_raster_data(matlab_raster_dir_name,
                             r_raster_dir_name, 
                             save_file_type = "csv",
                             files_contain = "bp1001spk_01A")
  
  csv_converted_file <- paste0(r_raster_dir_name, 
                             list.files(r_raster_dir_name, pattern = "csv"))
  
  # will return NULL if the converted data is correctly in raster format
  expect_null(test_valid_raster_format(csv_converted_file))
  
  
  
  # test saving converting files to rds
  convert_matlab_raster_data(matlab_raster_dir_name,
                             r_raster_dir_name, 
                             save_file_type = "rds",
                             files_contain = "bp1001spk_01A")
  
  rds_converted_file <- paste0(r_raster_dir_name, 
                               list.files(r_raster_dir_name, pattern = "rds"))
  
  # will return NULL if the converted data is correctly in raster format
  expect_null(test_valid_raster_format(rds_converted_file))
  
  

  # remove all files that were created
  
  the_files <- paste0(r_raster_dir_name, list.files(r_raster_dir_name))
  file.remove(the_files)
  #unlink(file.path(dirname(r_raster_dir_name), basename(r_raster_dir_name)), 
  #       recursive = TRUE, force = TRUE)
  
  
  
  
  
  
})




# testing calculating the number of times a label was repeated ----------------

test_that("get_num_label_repetitions() correctly assesses how many times a label was repeated", {

  file_name <-  system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package = "NeuroDecodeR")
  
  expect_error(get_num_label_repetitions(file_name, "stimulus_ID", levels_to_use = c("kiwi", "dog")))
  
  label_rep_info <- get_num_label_repetitions(file_name, "stimulus_ID") 
  
  plot(label_rep_info)  # make sure the plot function does not give an error
  
  expect_equal(length(get_siteIDs_with_k_label_repetitions(file_name, "stimulus_ID",  20)), 132)
  
})





# testing the function that checks if raster data is valid is working  ----------------

test_that("test_valid_raster_format() correctly assesses if data is in valid raster format", {
  
  raster_file_name <- paste0(raster_dir_name, list.files(raster_dir_name)[1])

  # valid raster file should be valid and return NULL
  expect_null(test_valid_raster_format(raster_file_name))
  
  # raster_data file name should only have a single object in it
  expect_error(test_valid_raster_format("example_DECODING_RESULTS.Rda"))
  
  # making sure test_valid_raster_format() correct tests that raster data has right variables 
  load(raster_file_name)
  expect_error(test_valid_raster_format(dplyr::select(raster_data, -.data$time)))
  expect_error(test_valid_raster_format(dplyr::select(raster_data, -.data$labels)))
  expect_error(test_valid_raster_format(dplyr::mutate(raster_data, blah = raster_data[, 1])))
  
  # make sure that one can plot the raster data
  plot.raster_data(raster_file_name)
  plot.raster_data(raster_file_name, facet_label = "stimulus_ID")
  
})




# testing the function that checks if binned data is valid is working  ----------------

test_that("test_valid_binned_format() correctly assesses if data is in valid binned format", {
  
  raster_file_name <- paste0(raster_dir_name, list.files(raster_dir_name)[1])
  binned_file_name <-  system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package = "NeuroDecodeR")
  
  # valid raster file should be valid and return NULL
  expect_null(test_valid_binned_format(binned_file_name))
  
  # binned_data file name should only have a single object in it
  expect_error(test_valid_binned_format("example_DECODING_RESULTS.Rda"))
  
  # making sure test_valid_binned_format() correct tests that raster data has right variables 
  load(binned_file_name)
  expect_error(test_valid_binned_format(dplyr::select(binned_data, -.data$siteID)))
  expect_error(test_valid_binned_format(dplyr::select(binned_data, -.data$time)))
  expect_error(test_valid_binned_format(dplyr::select(binned_data, -.data$labels)))
  expect_error(test_valid_binned_format(dplyr::mutate(binned_data, blah = binned_data[, 1])))
  
})



test_that("check_and_load_binned_data() priviate helper function is working", {
  
  raster_file_name <- paste0(raster_dir_name, list.files(raster_dir_name)[1])
  binned_file_name <-  system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), package = "NeuroDecodeR")
  
  # raster data is not in binned format so should produce an error
  expect_error(check_and_load_binned_data(raster_file_name))
  
  # check_and_load_binned_data() should return a data frame of binned data
  expect_true(is.data.frame(check_and_load_binned_data(binned_file_name)))  

})
