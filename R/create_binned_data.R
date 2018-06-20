#' @title A function that converts data from raster format to binned format
#'
#' @description This function takes takes a directory containing data in raster format
#' and converts it into a file that contains 'binned' spike count rate data
#' that is sampled at a particular frequency. 
#'
#' \describe{
#' \item{\code{create_binned_data(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_ind, end_ind, files_contain)}}{
#' creates a binned data file. 
#' }}
#'
#'
#' @param raster_directory_name character.  This argument is the name of a
#' directory that contains files that are in raster format.
#' @param save_prefix_name integer. This argument contains a prefix 
#' (e.g., directory name and additional characters) for how the resulting 
#' binned file should be saved. The string _binned_data_Xms_bins_Yms_sampled 
#' is appended on to the saved Rda file name, where X is the bin_width and
#' Y is the sampling interval. 
#' @param bin_width integer. The bin width over which spike count rates should be computed.
#' @param sampling_interval integer. The frequency with which the data should be sampled. 
#' @param start_ind integer. An optional argument that specifies the time index where the 
#' the first bin should start. 
#' @param end_ind logical. An optional argument that specifies the time index where the 
#' the binning process should end. 
#' @param files_contain character. Only raster data files that contain the file_contains 
#' string will be included in the binned data. 
#'
#' @examples
#' Bin the data using 150 ms bins sample at 50 ms intervals
#' Assumes that raster files are in the directory '../data/Zhang_Desimone_7objects_R_raster_data/' 
#' and saves the output file with the prefix ZD
#' \dontrun{
#' create_binned_data('../data/Zhang_Desimone_7objects_R_raster_data/', '../data/ZD', 150, 10)
#' 
#' 
#' # all options
#' create_binned_data(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_ind, end_ind, files_contain)
#' 
#' }
#' @import dplyr 
#' @import tictoc
#' @export




# bin the data for all sites
create_binned_data <- function(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL, files_contain = "") {
  
  
  # if the directory name does not end with a slash, add a slash to the directory name
  
  # if (raster_directory_name[length(raster_directory_name)] != '/')
  #  the above "matlab" expression acutally survives in linux, where two trailing slashed are recognized as one
  
  desired_pattern = '.*/$'
  if (grepl(desired_pattern, raster_directory_name) == FALSE){
    raster_directory_name <- paste0(raster_directory_name, '/')
  }  
  
  
  file.names <- list.files(raster_directory_name, pattern = files_contain)
  
  
  
  binned_data <- NULL
  for (i in 1:length(file.names)){
    
    cat(paste(i, " "))
    #print(i)
    #tic()
    
  
    binned_data_object_name <- load(paste0(raster_directory_name, file.names[i]))
    
    # checking for some backward compatibility, but make sure all data is a varaible called raster_data in the future
    if (binned_data_object_name == "raster_data"){
        # do nothing if the raster data is in a R object called raster_data
    } else if(binned_data_object_name == "raster.data"){
      raster_data <- raster.data   # rename raster.data to raster_data
    } else {
      stop("Data stored in raster files must contain an R object called raster_data")
    }
    
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
  
  save("binned_data", file = saved_binned_data_file_name, compress = TRUE)
  
  
}   # end function







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
    
    if (all_start_inds[i] == all_end_inds[i]){  # if binning at the same resolution as the original file, return original data
      binned_data_one_site[, i] <- spike_df[, all_start_inds[i]]
    } else{                                     # otherwise, actually bin the data
      binned_data_one_site[, i] <- rowMeans(spike_df[, all_start_inds[i]:all_end_inds[i]])   
    }
    
    
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












