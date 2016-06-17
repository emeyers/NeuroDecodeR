


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



# convertins rate data into count data (e.g., firing rates into spike counts)
convert.rates.to.counts <- function(binned.data) {


  the.data <- select(binned.data, starts_with("time"))
  the.labels <- select(binned.data, -starts_with("time"))
  all.dim.names <- names(binned.data)
  
  bin.widths <- get.bin.widths(names(the.data))
  
  data.counts <- sweep(the.data, 2, bin.widths, FUN = "*")

  
  # make sure the conversion worked, and that no value is 
  if (max(abs(round(data.counts) - data.counts)) > 10^-12) {
    stop('converting continuous activity to counts failed')
  }
  
  
  # add back the labels
  data.counts <- cbind(data.counts, the.labels)
  
  # put the data back in the original order
  data.counts <- data.counts[, all.dim.names]

  

  return(data.counts)
  
}





# gets how long a bin width is from data that is in binned.data format
get.bin.widths <- function(time.vector)
{
  
  bin.widths <- NULL
  
  for (i in 1:length(time.vector)){
    curr.parsed_names <- unlist(strsplit(as.character(time.vector[i]), ".", fixed = TRUE))
    bin.widths[i] <- as.numeric(curr.parsed_names[3]) - as.numeric(curr.parsed_names[2]) + 1
  }

  return(bin.widths)
  
}




# if there are ties int he maximum value, then this function returns an index of one of the maxes randomly
# this function was copied from the nnet package (which was slightly faster than my implementation)
rand.which.max <- function (x) 
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) 
    sample(y, 1L)
  else y
}





