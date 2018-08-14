#' A function that converts data from raster format to binned format
#'
#' ! link to raster format and binned format and related two functions
#'
#' 
#' @usage \code{create_binned_data(raster_dir_name, save_prefix_name, bin_width, sampling_interval, start_ind, end_ind, files_contain)}
#' 
#'
#'
#' @param raster_dir_name character. Name of a directory containing raster data in .Rda format.
#' @param save_prefix_name character. Prefix to the generated name for the created binned file, which is
#'  "\code{bin_width}_samples_binned_every_\code{sampling_interval}_samples
#' @param bin_width integer. The bin width over which raster data is averaged.
#' @param sampling_interval integer. It specifies the nth sample following the start of a bin, where the next bin starts 
#' @param start_ind integer. It specifies the time(ms)(i.e., the number behind "time.") where the binning process starts. 
#' It can be negative if your sample index is negative. By default, all data are included.
#' @param end_ind integer. It specifies the time(ms)(i.e., the number behind "time.") where the binning process should end by. 
#' It can be negative if your sample index is negative. By default, all data are included.
#' @param files_contain regular expression. Only raster data files that match the file_contains are binned.
#' @return Created binned data file will be written to disk. During execution, preceding the binning of each raster file, console spills the total number of raster files will have been binned (as you will see the number increments by one). After the creation of all files, console spills the binned file name. 
#' @examples
#' Bin the data using 150 sample bins sample at 50 sample intervals
#' Assumes that raster files are in the directory 'data/Zhang_Desimone_7objects_raster_data_rda/'
#' and saves the output file with the prefix ZD
#' \dontrun{
#' create_binned_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_rda/'), 'data/binned/ZD', 150, 10)
#' }
#' If you get other files mixed in the raster directory that are not .Rda files and only want to include data from 10th sample to 100th sample
#' \dontrun{
#' create_binned_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_rda/'), 'data/binned/ZD', 150, 10, -300, 300, "\\.Rda$")
#' }
#' @import dplyr
#' @export

# bin the data for all sites
create_binned_data <- function(raster_dir_name, save_prefix_name, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL,
                               files_contain = "") {
  # if the directory name does not end with a slash, add a slash to the directory name
  desired_pattern = '.*/$'
  if (grepl(desired_pattern, raster_dir_name) == FALSE){
    raster_dir_name <- paste0(raster_dir_name, '/')
  }  
  
  file_names <- list.files(raster_dir_name, pattern = files_contain)
  
  
  binned_data <- NULL
  binned_site_info <- NULL
  
  for (iSite in 1:length(file_names)) {
    cat(paste(iSite, " "))
    
    binned_data_object_name <- load(paste0(raster_dir_name, file_names[iSite]))
    
    if ((length(binned_data_object_name) == 2) && (match("raster_data", binned_data_object_name) + match("raster_site_info", binned_data_object_name) == 3)){
      
    } else {
      stop("Data stored in raster files must contain two R objets one called raster_data and another called raster_site_info...
           as in the new version or an R object called raster_data including site_info as in the old version")
    }
    
    
    
    
    one_binned_site <- bin_saved_data_one_site(raster_data, bin_width, sampling_interval, start_ind, end_ind)
    
    # append siteID to raster data, which is then appended to binned data
    one_binned_site$siteID <- rep(iSite, dim(one_binned_site)[1])
    binned_data <- rbind(binned_data, one_binned_site)
    
    # prepend siteID ro raster site info, which is then added to binned site info
    raster_site_info <- rlang::prepend(raster_site_info, setNames(as.list(iSite), "siteID"))
    binned_site_info[[iSite]]<- raster_site_info
  }
  
  # make the siteID be in the first column of binned dataa
  binned_data <- binned_data %>% select(siteID, everything())
  
  
  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(save_prefix_name, "_", bin_width, "_ms_binned_every_", sampling_interval,
                                        "_ms")
  start_time_name <- ""
  end_time_name <- ""
  
  if (!is.null(start_ind)) {
    start_time_name <- paste0("_start_", start_ind)
  }
  
  if (!is.null(end_ind)) {
    end_time_name <- paste0("_end_", end_ind)
  }
  
  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_time_name, end_time_name, ".Rda")
  print(saved_binned_data_file_name)
  save("binned_data", "binned_site_info", file = saved_binned_data_file_name, compress = TRUE)
}  # end function

# bin the data from one site
bin_saved_data_one_site <- function(raster_data, bin_width, sampling_interval, start_ind = NULL, end_ind = NULL) {
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
  
  return(dfCurr_site_binned_data)
}


