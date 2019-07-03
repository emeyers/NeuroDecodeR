# some utility functions that are useful


#' @export
get_center_bin_time <- function(time_vector) {
  center_bin_time <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), ".", fixed = TRUE))
    curr_parsed_names <- unlist(strsplit(as.character(curr_parsed_names[2]), "_", fixed = TRUE))
    center_bin_time[i] <- mean(as.numeric(curr_parsed_names[1:2]))  # length(curr.parsed_names[2]:curr.parsed_names[3])
  }
  
  return(center_bin_time)
}


#' @export
get_time_range_strings <- function(time_vector) {
  
  time_range_strings <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), ".", fixed = TRUE))
    curr_parsed_names <- unlist(strsplit(as.character(curr_parsed_names[2]), "_", fixed = TRUE))
    time_range_strings[i] <- paste(curr_parsed_names[1], 'to', curr_parsed_names[2])  
  }
  
  return(time_range_strings)
}




#' @export
# converts rate data into count data (e.g., firing rates into spike counts)
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





#' @export
# gets how long a bin width is from data that is in binned.data format
get_bin_widths <- function(time_vector) {
  
  bin_widths <- NULL
  
  for (i in 1:length(time_vector)) {
    curr_parsed_names <- unlist(strsplit(as.character(time_vector[i]), "[._]"))
    bin_widths[i] <- as.numeric(curr_parsed_names[3]) - as.numeric(curr_parsed_names[2]) + 1
  }
  
  return(bin_widths)
}





#' @export
# if there are ties int he maximum value, then this function returns an index of one of the maxes randomly this function
# was copied from the nnet package (which was slightly faster than my implementation)
rand_which_max <- function(x) {
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    sample(y, 1L)
  } else {
    y
  } 
}