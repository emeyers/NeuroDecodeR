#' A function that create binned data in .Rda format from raster data in .mat format
#' 
#' ! link to raster format and binned format and related two functions
#' 
#' #' @usage \code{create_binned_data(raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_df_ind, end_df_ind, files_contain)}
#' 
#'
#' @param matlab_directory_name character. Name of a directory containing raster data in .mat format.
#' @param save_prefix_name character. Prefix to the generated name for the created binned file, which is
#'  "\code{bin_width}_samples_binned_every_\code{sampling_interval}_samples
#' @param bin_width integer. The bin width over which raster data is averaged.
#' @param sampling_interval integer. It specifies the nth sample following the start of a bin, where the next bin starts 
#' @param start_df_ind integer. It specifies the column index where the binning process starts. It must not be negative. By default, all data is included.
#' @param end_df_ind integer. It specifies the column index where the binning process should end by. It must not be negative. By default, all data is included.
#' @param files_contain regular expression. Only raster data files that match the file_contains are binned.
#' @return Preceding the binning of each raster file, it spills the total number of raster files will have been binned as you will see
#' the number increments by one. After the creation of all files, it spills the binned file name. By default, it is an empty character.
#' @examples
#' Bin the data using 150 sample bins sample at 50 sample intervals
#' Assumes that raster files are in the directory 'data/Zhang_Desimone_7objects_raster_data_mat/'
#' and saves the output file with the prefix ZD
#' \dontrun{
#' create_binned_data_from_matlab_raster_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_mat/'), 'data/binned/ZD', 150, 10)
#' }
#' If you get other files mixed in the raster directory that are .mat files and only want to include data from 10th sample to 100th sample
#' \dontrun{
#' create_binned_data_from_matlab_raster_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_mat/'), 'data/binned/ZD', 150, 10, 10, 800, "\\.mat$")
#' }
#' @import R.matlab
#' @import tidyr
#' @import dplyr
#' @import rlang
#' @export



create_binned_data_from_matlab_raster_data <- function(matlab_raster_directory_name, save_prefix_name, bin_width, sampling_interval, start_df_ind = NULL, end_df_ind = NULL,
                                                       files_contain = "") {
  start_ind_name <- ""
  end_ind_name <- ""
  

  
  
  matlab_file_names <- list.files(matlab_raster_directory_name, pattern = files_contain, full.names = TRUE)
  binned_site_info <- NULL
  binned_data <- NULL
  
  for (iSite in 1:length(matlab_file_names)) {
    cat(paste(iSite, " "))
    
    
    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]
    
    raster <- R.matlab::readMat(curr_matlab_file_name)
    
    
    
    # second, create the raster_site_info list and add to binned_site_info
    
    raster_site_info <- raster$raster.site.info[,,1]
    
    # prepend siteID ro raster site info, which is then added to binned site info
    raster_site_info <- rlang::prepend(raster_site_info, setNames(as.list(iSite), "siteID"))
    binned_site_info[[iSite]]<- raster_site_info
    
    
    
    
    # third, create the raster_data df and bin it
    # Get the raster data
    raster_data <- data.frame(raster$raster.data)
    
    start_ind_name <- ""
    end_ind_name <- ""
    
    if (is.null(start_df_ind)) {
      start_df_ind <- 1
    } else{
      start_ind_name <- paste0("_start_", start_df_ind)
    }
    
    if (is.null(end_df_ind)) {
      end_df_ind <- dim(raster_data)[2]
    } else{
      
      end_ind_name <- paste0("_end_", end_df_ind)
    }
    
    
    raster_data <- raster_data[,start_df_ind:end_df_ind]
    
    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_times <- 1:dim(raster_data)[2]
    
    # if there is an alignment time, subtract it from the raster times
    if (sum(names(raster_site_info) == "alignment.event.time")) {
      data_times <- (data_times - raster_site_info$alignment.event.time)
    }
    
    names(raster_data) <- paste0("time.", data_times)
    dfCurr_site_binned_data <- bin_data_one_site(raster_data, bin_width, sampling_interval)
    
    
    # forth, add the labels to dfCurr_site_binned_data and add it to binned_data
    # Get the labels for what happened on each trial and add them to the raster.data data frame
    raster_labels <- raster$raster.labels
    # loop over label names
    all_var <- convert_dot_back_to_underscore(row.names(raster_labels))
    
    for (iVar in 1:length(all_var)) {
      # get the name for the current raster_labels
      curr_var_name <- all_var[iVar]
      # add the prefix labels. to the curr label name...
      curr_var_name <- paste0("labels.", curr_var_name)
      # levels are contained in an extra list - remove this extra list to get the vector of names
      curr_levels <- raster_labels[iVar, , ][[1]]
      curr_levels <- sapply(curr_levels, function(x) x[[1]])
      # put into a data frame with the appropriate column name
      curr_var_column <- eval(parse(text = paste0("curr_var_column <- data.frame(", curr_var_name, " = curr_levels)")))
      # add to the raster.data raster_data <- cbind(raster_data, curr_var_column)
      dfCurr_site_binned_data <- cbind(curr_var_column, dfCurr_site_binned_data)
    }
    
    # append siteID to raster data, which is then appended to binned data
    dfCurr_site_binned_data$siteID <- rep(iSite, dim(dfCurr_site_binned_data)[1])
    binned_data <- rbind(binned_data, dfCurr_site_binned_data)
    
  }
  
  # make the siteID be in the first column of binned dataa
  binned_data <- binned_data %>% select(siteID, everything())
  
  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(save_prefix_name, "_", bin_width, "_samples_binned_every_", sampling_interval,
                                        "_samples")

  
  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_ind_name, end_ind_name, ".Rda")
  print(saved_binned_data_file_name)
  save("binned_data", "binned_site_info", file = saved_binned_data_file_name, compress = TRUE)
  
}  # end function

bin_data_one_site <- function(spike_df, bin_width, sampling_interval) {
  
  
  

  all_start_df_inds <- seq(1, dim(spike_df)[2] - (bin_width - 1), by = sampling_interval)
  all_end_df_inds <- all_start_df_inds + (bin_width - 1)
  dfCurr_site_binned_data <- as.data.frame(matrix(nrow = dim(spike_df)[1], ncol = length(all_start_df_inds)))
  
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
  
  names(dfCurr_site_binned_data) <- paste0("time.", all_start_df_inds, "_", all_end_df_inds)
  
  return(dfCurr_site_binned_data)
}

convert_dot_back_to_underscore <- function(oldnames){
  newnames = gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
}