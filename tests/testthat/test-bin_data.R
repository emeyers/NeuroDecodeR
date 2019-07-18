

raster_dir_name <- file.path('..', '..', 'data', 'raster', 'Zhang_Desimone_7objects_raster_data_rda')



# a helper function that assess if binned data is in a valid format
test_binned_data_is_in_valid_format <- function(binned_file_name) {
  
  load(binned_file_name)
  
  expect_true(exists("binned_site_info"))
  expect_true(exists("binned_data"))
  
  unique_prefixes <- unique(sapply(strsplit(names(binned_data), '[.]'), function(x) x[1]))
  
  expect_equal(unique_prefixes, c("siteID", "labels", "time"))  
  
}



# testing create_binned_data --------------------------------------------------

name_of_file_that_should_be_created <- "ZD_150_samples_binned_every_50_samples.Rda"

# deleting "ZD_150_samples_binned_every_50_samples.Rda" if it already exist
if (file.exists(name_of_file_that_should_be_created)){
  file.remove(name_of_file_that_should_be_created)
} 


binned_file_name <- create_binned_data(raster_dir_name, "ZD", 150, 50)
expect_equal(name_of_file_that_should_be_created, binned_file_name)


test_binned_data_is_in_valid_format(binned_file_name)

file.remove(name_of_file_that_should_be_created)


















