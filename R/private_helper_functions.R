
# This file contains helper functions that will not be publicly visible, but
#  instead are used interally by other functions in the NDTr





# Takes a vector of strings where each string is in the format of time.X_Y and
# calculates the mean of X and Y. This is primarily used by the result metric
# plot() functions to label the time axes on plots.

get_center_bin_time <- function(time_vector) {
  center_bin_time <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), ".", fixed = TRUE))
    curr_parsed_names <- unlist(strsplit(as.character(curr_parsed_names[2]), "_", fixed = TRUE))
    center_bin_time[i] <- mean(as.numeric(curr_parsed_names[1:2]))  # length(curr.parsed_names[2]:curr.parsed_names[3])
  }
  
  return(center_bin_time)
}




# Takes a vector of strings where each string is in the format of time.X_Y and
# calculates returns strings in the format "X to Y". This is primarily used by
# the result metric plot() functions to label the time axes on plots.

get_time_range_strings <- function(time_vector) {
  
  time_range_strings <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), ".", fixed = TRUE))
    curr_parsed_names <- unlist(strsplit(as.character(curr_parsed_names[2]), "_", fixed = TRUE))
    time_range_strings[i] <- paste(curr_parsed_names[1], 'to', curr_parsed_names[2])  
  }
  
  return(time_range_strings)
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
  data_counts <- data_counts[, all_dim_names]   # put data back to the original order
  
  return(data_counts)
}






# Gets how long a bin width is from data that is in binned_data format. This is
# used by the convert_rates_to_counts() function above to convert firing rates
# to spike counts.

get_bin_widths <- function(time_vector) {
  
  bin_widths <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), "[._]"))
    bin_widths[i] <- as.numeric(curr_parsed_names[3]) - as.numeric(curr_parsed_names[2]) + 1
  }
  
  return(bin_widths)
}






# If there are ties in the maximum value, then this function returns an index of
# one of these maximum values randomly (this function was copied from the nnet
# package which had a fast implementation)

rand_which_max <- function(x) {
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    sample(y, 1L)
  } else {
    y
  } 
}





# creates an ID based on the date, time, and a random number so 
# that different analyses can be uniquely identified by this number.
# This is primarily used by the cross-validation object to uniquely identify each
# analysis and by the functions in the save_and_manage_decoding_results to 
# save each result with an unique ID.

generate_analysis_ID <- function(){
  
  
  # create a name for the file that will hold the results
  curr_time <- as.character(Sys.time())
  curr_time <- gsub("-", "", curr_time)
  curr_time <- gsub(":", "", curr_time)
  curr_time <- gsub(" ", "_", curr_time)
  rand_suffix <- paste0(round(runif(5, 0, 9)), collapse = "")
  
  analysis_ID <- paste(curr_time, rand_suffix, sep = "_")  
  
  analysis_ID
  
}







