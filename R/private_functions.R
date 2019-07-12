

# contains helper functions that will not be publicly visable


# creates an ID based on the date, time, and a random number so 
# that different analyses can be identified by this number
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














