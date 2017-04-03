






# 
# # examples of how the code is used
# 
# raster_directory_name <- '../data/Zhang_Desimone_7objects_R_raster_data/'
# save_prefix_name <- '../data/ZD'
# bin_width <- 150   #100
# sampling_interval <- 50   #10
# 
# create_binned_data(raster_directory_name, save_prefix_name, bin_width, sampling_interval)
# 
# 
# # all options...
# # create_binned_data(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_ind, end_ind, files_contain)










library(dplyr)
library(tictoc)



# bin the data from one site
bin_data_one_site <- function(raster.data, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL){
  
  
  labels_df <- dplyr::select(raster.data, -starts_with("time"))
  spike_df <- dplyr::select(raster.data, starts_with("time"))
  
  if (is.null(start_ind))
    start_ind <- 1
  
  if (is.null(end_ind))
    end_ind <- dim(spike_df)[2]
  
  all_start_inds <- seq(start_ind, end_ind - bin_width + 1, by = sampling_interval) 
  all_end_inds <- all_start_inds + bin_width - 1
  
  
  binned_data_one_site <- as.data.frame(matrix(nrow = dim(raster.data)[1], ncol = length(all_start_inds)))
  for (i in 1:length(all_start_inds)){
    binned_data_one_site[, i] <- rowMeans(spike_df[, all_start_inds[i]:all_end_inds[i]])
  }
  
  
  names(binned_data_one_site) <- paste0('time.', all_start_inds, '.', all_end_inds)
  
  binned_data_one_site <- cbind(labels_df, binned_data_one_site)
  
  
  return(binned_data_one_site)
  
}






# attempt at a faster version...
bin_data_one_site2 <- function(raster.data, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL){
  
  
  labels_df <- dplyr::select(raster.data, -starts_with("time"))
  spike_df <- dplyr::select(raster.data, starts_with("time"))
  
  if (is.null(start_ind))
    start_ind <- 1
  
  if (is.null(end_ind))
    end_ind <- dim(spike_df)[2]
  

  binned_data_one_site <- as.data.frame(matrix(nrow = dim(raster.data)[1], ncol = length(all_start_inds)))
 
  
  # start by smoothing the data to create the binned data with a boxcar filter
  the_filter <- rep(1, bin_width)/bin_width
  
  # use apply to get out smoothed data
  binned_data_one_site <- data.frame(t(apply(raster.data, 1, stats::filter, the_filter)))
  
  
  sampling_inds <- seq(start_ind, end_ind, by = sampling_interval)
  binned_data_one_site <- binned_data_one_site[, sampling_inds]
  
  
  all_start_inds <- seq(start_ind, end_ind - bin_width + 1, by = sampling_interval) 
  all_end_inds <- all_start_inds + bin_width - 1
  
  names(binned_data_one_site) <- paste0('time.', all_start_inds, '.', all_end_inds)
  
  binned_data_one_site <- cbind(labels_df, binned_data_one_site)
  
  return(binned_data_one_site)
  
}




# bin the data for all sites
create_binned_data <- function(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL, files_contain = "") {

  
  # if the directory name does not end with a slash, add a slash to the directory name
  if (raster_directory_name[length(raster_directory_name)] != '/')
    raster_directory_name <- paste0(raster_directory_name, '/')


  file.names <- list.files(raster_directory_name, pattern = files_contain)
  
  

  binned_data <- NULL
  for (i in 1:length(file.names)){

    print(i)
    #tic()
    
    
    load(paste0(raster_directory_name, file.names[i]))
    
    
    # checking for some backward compatibility, but make sure all data is raster_data in the future
    # if (!exists('raster_data'))
    #  raster.data <- raster.data
    
    
    
    one_binned_site <- bin_data_one_site(raster_data, bin_width, sampling_interval, start_ind, end_ind)
    
    one_binned_site$siteID <- rep(i, dim(one_binned_site)[1])
    
    binned_data <- rbind(binned_data, one_binned_site)
    
    #toc()
    
  }
  
  

  # make the siteID be in the first column
  binned_data <- binned_data %>% select(siteID, everything())
  
  
  #return(binned_data)
  
  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(save_prefix_name, "_", "binned_data_", bin_width, "ms_bins_", sampling_interval, "ms_sampled")
 
  start_time_name <- ""
  end_time_name <- ""
  
  if (!is.null(start_ind))
      start_time_name <- paste0("_start_", start_ind)
      
  if (!is.null(end_ind))
    end_time_name <- paste0("_end_", end_ind)
   
  
  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_time_name, end_time_name, ".Rda")

  print(saved_binned_data_file_name)
  
  save("binned_data", file = saved_binned_data_file_name)
  
  
}   # end function















