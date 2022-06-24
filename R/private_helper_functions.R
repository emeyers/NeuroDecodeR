
# This file contains helper functions that will not be publicly visible, but
#  instead are used internally by other functions in the NDR





# Checks if all time bins have an start and end time;
#  i.e., all time bins are in the format time.XXX_YYY
#  Returns TRUE if all time bins have an end time.
check_raster_data_contains_end_times <- function(raster_data){
  
  spike_df <- dplyr::select(raster_data, starts_with("time"))
  
  start_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[1]))
  end_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[2]))
  
  # return TRUE if all time bins have an end time
  sum(!is.na(end_time_values)) == length(end_time_values)  
  
}




# Add the end times to raster_data that only has start times, e.g., raster_data
#   that has time columns in the format time.XXX rather than time.XXX_YYY. 
add_raster_data_end_times <- function(raster_data) {
  
  
  # if the data already has end times just return the original data
  if (check_raster_data_contains_end_times(raster_data)) {
    message("The raster_data already contains end times. Just returning the original data.")
    return(raster_data)
  }
  
  
  labels_df <- dplyr::select(raster_data, -starts_with("time"))
  spike_df <- dplyr::select(raster_data, starts_with("time"))
  
  start_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[1]))
  end_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[2]))
  
  
  
  # should only be one value since we are assuming that all the time bins are of the same length
  sampling_interval_length <- unique(diff(start_time_values))  
  
  if (length(sampling_interval_length) > 1) {
    stop("Could not infer bin end times because the start bin times are not at equal interval. Please
         manually specify your end times so that your time bin columns are named in the format time.XXX_YYY")
  }
  
  
  
  # if only some of the end times are missing, let the user know that only some of the end times will be filled in
  total_num_times <- length(end_time_values)
  num_end_times_missing <- sum(is.na(end_time_values))
  if (num_end_times_missing != total_num_times) {    # already checked that not all the end times are present
    message("Some of the time bins contain end times. Going to replace only the end times that are missing.")
  }  
  
  
  filled_in_end_time_values <- start_time_values + sampling_interval_length
  new_end_time_values <- end_time_values
  
  # If one wants to replace all end times that already exist, comment out this line
  new_end_time_values[is.na(end_time_values)] <- filled_in_end_time_values[is.na(end_time_values)]
  
  
  
  # rebuild the data set with the new names and return it
  new_time_interval_names <- paste0("time.", start_time_values, "_", new_end_time_values)
  names(spike_df) <- new_time_interval_names
  
  
  # bind the data back together, and the raster_data attribute, and return the data
  new_raster_data <- cbind(labels_df, spike_df)
  
  attr(new_raster_data, "class") <- c("raster_data", "data.frame")
  
  new_raster_data
  
}








# This function can take either a string containing a file name to data in
# binned format or an actual data frame in binned format. If the argument is a
# string then the data is loaded from the file name. The binned data is checked
# to make sure it is valid binned data format and then it is returned.
check_and_load_binned_data <- function(binned_data) {

  # if a file name has been given load it
  if (is.character(binned_data)) {
    binned_data_object_name <- load(binned_data)
    eval(parse(text = paste0("binned_data <- ", binned_data_object_name)))
  }


  # check that the binned_data is in a valid format
  result <- tryCatch({
      test_valid_binned_format(binned_data)
    },
    error = function(e) {
      stop(paste(
        "The argement binned_data must either be a data frame containing data in binned format,",
        "or a string listing a path to a file that has data in binned format.",
        "Use NeuroDecodeR:::test_valid_binned_format(binned_data) for more information on",
        "how the data is not conforming to the binned format."))
   })


  binned_data
  
}





# Takes a vector of strings where each string is in the format of time.X_Y and
# calculates the mean of X and Y. This is primarily used by the result metric
# plot() functions to label the time axes on plots.

get_center_bin_time <- function(time_vector) {
  
  start_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", time_vector), "_"), function(l) l[1]))
  end_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", time_vector), "_"), function(l) l[2]))
  
  (start_time_values + end_time_values)/2
  
}
  
  



# Takes a vector of strings where each string is in the format of time.X_Y and
# calculates returns strings in the format "X to Y". This is primarily used by
# the result metric plot() functions to label the time axes on plots.

get_time_range_strings <- function(time_vector) {
  
  start_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", time_vector), "_"), function(l) l[1]))
  end_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", time_vector), "_"), function(l) l[2]))
  
  #paste(start_time_values, "to", end_time_values)
  paste0("[", start_time_values, ", ", end_time_values, ")")
  
}







# Converts rate data into count data (e.g., firing rates into spike counts).
#  This is primarily used by the ds_basic object to convert firing rates to
#  counts so that the poisson_naive_bayes classifier will work.

convert_rates_to_counts <- function(binned_data) {
  
  the_data <- select(binned_data, starts_with("time"))
  the_labels <- select(binned_data, -starts_with("time"))
  all_dim_names <- names(binned_data)
  bin_widths <- get_bin_widths(names(the_data))
  data_counts <- sweep(the_data, 2, bin_widths, FUN = "*")

  # make sure the conversion worked, and that no value is
  if (max(abs(round(data_counts) - data_counts)) > 10^-12) {
    stop("converting continuous activity to counts failed")
  }

  data_counts <- cbind(data_counts, the_labels) # add back the labels
  data_counts <- data_counts[, all_dim_names] # put data back to the original order

  return(data_counts)
}






# Gets how long a bin width is from data that is in binned_data format. This is
# used by the convert_rates_to_counts() function above to convert firing rates
# to spike counts.

get_bin_widths <- function(time_vector) {
  
  bin_widths <- NULL

  for (i in seq_along(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), "[._]"))
    bin_widths[i] <- as.numeric(curr_parsed_names[3]) - as.numeric(curr_parsed_names[2])  # + 1
  }

  return(bin_widths)
}






# If there are ties in the maximum value, then this function returns an index of
# one of these maximum values randomly selected (this function was copied from
# the nnet package which had a fast implementation)

rand_which_max <- function(x) {
  
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    sample(y, 1L)
  } else {
    y
  }
  
}





# creates an ID based on the date, time, and a random number so that different
# analyses can be uniquely identified by this number. This is primarily used by
# the cross-validation object to uniquely identify each analysis and by the
# functions in the save_and_manage_decoding_results to save each result with an
# unique ID.

generate_analysis_ID <- function() {

  # create a name for the file that will hold the results
  curr_time <- as.character(Sys.time())
  curr_time <- gsub("-", "", curr_time)
  curr_time <- gsub(":", "", curr_time)
  curr_time <- gsub(" ", "_", curr_time)
  rand_suffix <- paste0(round(stats::runif(5, 0, 9)), collapse = "")

  analysis_ID <- paste(curr_time, rand_suffix, sep = "_")

  analysis_ID
}
