


# some utility functions that are useful


get.center.bin.time <- function(time.vector)
{
  
  center_bin_time <- NULL
  
  for (i in 1:length(time.vector)){
    curr.parsed_names <- unlist(strsplit(as.character(time.vector[i]), ".", fixed = TRUE))
    center_bin_time[i] <- mean(as.numeric(curr.parsed_names[2:3]))    # length(curr.parsed_names[2]:curr.parsed_names[3])
  }
  
  
  return(center_bin_time)
  
}


















