

raster_dir_name <- file.path('..', '..', 'data', 'raster', 'Zhang_Desimone_7objects_raster_data_rda')



# a function that assess if binned data is in a valid format
test_binned_data_is_in_valid_format <- function(binned_file_name) {
  
  load(binned_file_name)
  
  expect_true(exists("binned_site_info"))
  expect_true(exists("binned_data"))
  
  unique_prefixes <- unique(sapply(strsplit(names(binned_data), '[.]'), function(x) x[1]))
  
  expect_equal(unique_prefixes, c("siteID", "labels", "time"))  
  
}




# a function that assess if raster data is in a valid format
test_raster_data_is_in_valid_format <- function(raster_file_name) {
  
  load(raster_file_name)
  
  expect_true(exists("raster_site_info"))
  expect_true(exists("raster_data"))
  
  unique_prefixes <- sort(unique(sapply(strsplit(names(raster_data), '[.]'), function(x) x[1])))
  
  expect_equal(unique_prefixes[1:2], c("labels", "time"))  
  
}







# testing create_binned_data --------------------------------------------------

name_of_file_that_should_be_created <- "ZD_150_samples_binned_every_50_samples.Rda"

# deleting "ZD_150_samples_binned_every_50_samples.Rda" if it already exist
if (file.exists(name_of_file_that_should_be_created)){
  file.remove(name_of_file_that_should_be_created)
} 


# binned_file_name <- create_binned_data(raster_dir_name, "ZD", 150, 50)
binned_file_name <- create_binned_data(raster_dir_name, "ZD", 150, 50, 
                                       files_contain = "bp1001spk")

expect_equal(name_of_file_that_should_be_created, binned_file_name)


test_binned_data_is_in_valid_format(binned_file_name)

file.remove(name_of_file_that_should_be_created)




# testing converting MATLAB raster data to R raster data ----------------------

matlab_raster_dir_name <- file.path('..', '..', 'data', 'raster', 
                                    'Zhang_Desimone_7objects_raster_data_mat', '')


# create temporary directory to hold converted data
r_raster_dir_name <- file.path("test_convert_matlab_raster_data", "")

if (file.exists(r_raster_dir_name)){
  # delete any saved results and manifest files that already exist
  the_files <- paste0(r_raster_dir_name, list.files(r_raster_dir_name))
  file.remove(the_files)
  file.remove(r_raster_dir_name)
  
} 

dir.create(r_raster_dir_name)



r_raster_dir_name <- convert_matlab_raster_data(matlab_raster_dir_name, 
                                                r_raster_dir_name, 
                                                files_contain = "bp1001spk")

# r_raster_dir_name <- convert_matlab_raster_data(matlab_raster_dir_name,
#                                                start_ind = 300,
#                                                start_ind = 800,
#                                                files_contain = "bp1001spk")



a_converted_file <- paste0(r_raster_dir_name, list.files(r_raster_dir_name)[1])
  
test_raster_data_is_in_valid_format(a_converted_file)



the_files <- paste0(r_raster_dir_name, list.files(r_raster_dir_name))
file.remove(the_files)
file.remove(r_raster_dir_name)












