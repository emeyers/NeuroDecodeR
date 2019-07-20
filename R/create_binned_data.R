#' A function that converts data from raster format to binned format
#'
#' This function takes the name of a directory that contains files in raster
#' format, and averages the data within a specified bin size at specifid
#' sampling inverval increments to create data in binned format used for
#' decoding.
#'
#'
#' @param raster_dir_name A string that contains the path to a directory that
#'   has files in raster format. These files will be combined into binned format
#'   data.
#'
#' @param save_prefix_name A string with a prefix that will be used name of file
#'   that contains the saved binned format data.
#' 
#' @param bin_width A number that has the bin width that data should be averaged over. 
#' 
#' @param sampling_interval A number that has the specifies the sampling
#'   interval between successive binned-data points.
#' 
#' @param start_ind A number that specifies at which index should the binning
#'   start. By default it starts at the beginning time in the raster data.
#' 
#' @param end_ind A number that specifies at which index should the binning
#'   end. By default it end at the end time in the raster data.
#' 
#' @param files_contain A string that specifies that only raster files that
#'   contain this string should be included in the binned format data.
#' 
#' @examples 
#' # create binned data with 150 ms bin sizes sampled at 10 ms intervals
#' raster_dir_name <- file.path('..', '..', 'data', 'raster', 
#'                              'Zhang_Desimone_7objects_raster_data_rda')
#'                              
#' binned_file_name <- create_binned_data(raster_dir_name, "ZD", 150, 50)
#' 
#' 






#' @export
create_binned_data <- function(raster_dir_name, 
                               save_prefix_name, 
                               bin_width, 
                               sampling_interval, 
                               start_ind = NULL, 
                               end_ind = NULL,
                               files_contain = "") {
  
  
  # if the directory name does not end with a slash, add a slash to the directory name
  desired_pattern = '.*/$'
  if (grepl(desired_pattern, raster_dir_name) == FALSE){
    raster_dir_name <- paste0(raster_dir_name, '/')
  }  
  
  
  file_names <- list.files(raster_dir_name, pattern = files_contain)
  
  
  binned_data <- NULL
  binned_site_info <- NULL
  
  
  # loop through all raster data files and bin them
  for (iSite in 1:length(file_names)) {
    
    cat(paste(iSite, " "))
    
    binned_data_object_name <- load(paste0(raster_dir_name, file_names[iSite]))
    
    if (!((length(binned_data_object_name) == 2) && (match("raster_data", binned_data_object_name) + match("raster_site_info", binned_data_object_name) == 3))){
      stop(paste0('Data stored in raster files must contain two R objets one called "raster_data"', 
           'and another called "raster_site_info"'))
    }
    
    one_binned_site <- bin_saved_data_one_site(raster_data, bin_width, sampling_interval, start_ind, end_ind)
    
    # append siteID to raster data, which is then appended to binned data
    one_binned_site$siteID <- rep(iSite, dim(one_binned_site)[1])
    binned_data <- rbind(binned_data, one_binned_site)
    
    # prepend siteID or raster site info, which is then added to binned site info
    #raster_site_info <- rlang::prepend(raster_site_info, setNames(as.list(iSite), "siteID"))
    raster_site_info$siteID <- iSite  # prepend() is deprecated so using this instead

    binned_site_info[[iSite]]<- raster_site_info
  }
  
  
  # make the siteID be in the first column of binned dataa
  binned_data <- binned_data %>% select(siteID, everything())
  
  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(save_prefix_name, "_", bin_width, "_samples_binned_every_", sampling_interval,
                                        "_samples")
  start_time_name <- ""
  end_time_name <- ""
  
  if (!is.null(start_ind)) {
    start_time_name <- paste0("_start_", start_ind)
  }
  
  if (!is.null(end_ind)) {
    end_time_name <- paste0("_end_", end_ind)
  }
  
  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_time_name, end_time_name, ".Rda")
  save("binned_data", "binned_site_info", file = saved_binned_data_file_name, compress = TRUE)
  
  saved_binned_data_file_name
    
  
}  # end function






# A helper function for create_binned_data() to bin the data from one site
bin_saved_data_one_site <- function(raster_data, 
                                    bin_width, 
                                    sampling_interval, 
                                    start_ind = NULL, 
                                    end_ind = NULL) {
  
  
  labels_df <- dplyr::select(raster_data, -starts_with("time"))
  spike_df <- dplyr::select(raster_data, starts_with("time"))
  
  # start_ind is what you have beheind "time."
  # start_time is the whole time label
  # start_df_ind is the index of the time column in spike_df
  # start_df_ind and start_ind are the same if the time starts at 1
  
  if (is.null(start_ind)) {
    start_ind <- as.numeric(gsub("time.", "", colnames(spike_df)[1]))
  } 
  start_time <- paste0("time.", start_ind)
  start_df_ind <- match(start_time, colnames(spike_df))
  
  
  if (is.null(end_ind)) {
    temp_length <- dim(spike_df)[2]
    end_ind <- as.numeric(gsub("time.", "", colnames(spike_df)[temp_length]))
  } 
  
  end_time <- paste0("time.", end_ind)
  end_df_ind <- match(end_time, colnames(spike_df))
  
  all_start_df_inds <- seq(start_df_ind, end_df_ind - (bin_width - 1), by = sampling_interval)
  all_end_df_inds <- all_start_df_inds + (bin_width - 1)
  dfCurr_site_binned_data <- as.data.frame(matrix(nrow = dim(raster_data)[1], ncol = length(all_start_df_inds)))
  
  
  for (iBin in 1:length(all_start_df_inds)) {
    
    if (all_start_df_inds[iBin] == all_end_df_inds[iBin]) {
      # if binning at the same resolution as the original file, return original data
      # add start_df_ind to offset the prestimlus time
      dfCurr_site_binned_data[, iBin] <- spike_df[, all_start_df_inds[iBin]]
    
    } else {
      
      # otherwise, actually bin the data
      dfCurr_site_binned_data[, iBin] <- rowMeans(spike_df[, all_start_df_inds[iBin] :all_end_df_inds[iBin] ])
    }
  }
  
  names(dfCurr_site_binned_data) <- paste0("time.", all_start_df_inds + (start_ind - start_df_ind), "_", all_end_df_inds + (end_ind - end_df_ind))
  dfCurr_site_binned_data <- cbind(labels_df, dfCurr_site_binned_data)
  
  dfCurr_site_binned_data
  
}



