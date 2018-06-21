# # clear all the variables in memory before running # rm(list = ls())

# not implemented yet: [start_time], [end_time])
create_binned_data_from_matlab_raster_data <- function(raster_directory_name, save_prefix_name, bin_width, sampling_interval) {
  
  require("R.matlab")
  require("tidyr")
  require("dplyr")
  
  # helper funciton: every list in raster_labels is contained in an extra list the function below removes this extra list
  # so that it will just return the items of interest
  delist <- function(data_in_list) sapply(data_in_list, function(x) x[[1]])
  file_names <- list.files(raster_directory_name)
  binned_site_info <- NULL
  binned_data <- NULL
  
  for (siteID in 1:length(file_names)) {
    print(siteID)
    # current file name
    curr_matlab_file_name <- file_names[siteID]
    raster_data_list_imported_from_matlab <- readMat(paste(raster_directory_name, curr_matlab_file_name, sep = ""))
    # parse the raster site info...
    raster_site_info <- data.frame(raster_data_list_imported_from_matlab$raster.site.info)
    raster_site_info_names <- row.names(raster_site_info)
    
    for (iSiteInfo in 1:length(raster_site_info_names)) {
      curr_info_data <- unlist(raster_site_info[iSiteInfo, ])
      eval(parse(text = (paste0("binned_site_info$", eval(raster_site_info_names[iSiteInfo]), "[", eval(siteID), "] <- curr_info_data"))))
    }
    
    # parse the raster data
    raster_data <- data.frame(raster_data_list_imported_from_matlab$raster.data)
    # start by smoothing the data to create the binned data with a boxcar filter
    the_filter <- rep(1, bin_width)/bin_width
    # use apply to get out smoothed data
    binned_data_one_site <- data.frame(t(apply(raster_data, 1, stats::filter, the_filter)))
    # down-sample the data now to save on memory...
    start_ind <- max(which(is.na(binned_data_one_site[1, 1:bin_width]))) + 1
    end_ind <- min(which(is.na(binned_data_one_site[1, bin_width:ncol(binned_data_one_site)]))) + bin_width - 1
    sampling_inds <- seq(start_ind, end_ind, by = sampling_interval)
    binned_data_one_site <- binned_data_one_site[, sampling_inds]
    
    # alternatively - rather than listing the whole interval, can just list the center of the bin...  library('stringr')
    # names(binned_data_one_site) <- str_replace(names(binned_data_one_site), 'X', 'time_')
    
    # create better labels for the columns specifying the time bins
    if (sum(raster_site_info_names == "alignment.event.time") > 0) {
      alignment.event.time <- as.numeric(raster_site_info["alignment.event.time", ])
    } else {
      alignment.event.time <- 0
    }
    
    start_time_labels <- (sampling_inds - bin_width/2 + 1) - alignment.event.time
    end_time_labels <- (sampling_inds + bin_width/2) - alignment.event.time
    names(binned_data_one_site) <- paste0("time_", start_time_labels, "_", end_time_labels)
    
    # can parse the times later using: unlist(strsplit(x, '_', fixed = TRUE))
    # can plot things to show it worked (just need to make screen larger) plot(apply(binned_data_one_site, 2, mean, na.rm =
    # TRUE), type = 'l')
    
    # add in the site's ID
    siteID_df <- data.frame(siteID = rep(siteID, dim(binned_data_one_site)[1]))
    binned_data_one_site <- cbind(siteID = siteID_df, binned_data_one_site)
    # add labels...
    raster_labels <- raster_data_list_imported_from_matlab$raster.labels
    # loop over label names
    label_names <- row.names(raster_labels)
    
    for (iLabel in 1:length(label_names)) {
      # get the name for the current raster_labels
      curr_label_name <- eval(label_names[iLabel])
      # add the prefix labels_ to the curr label name...
      curr_label_name <- paste("labels_", curr_label_name, sep = "")
      # extract the labels themselves...
      curr_labels <- raster_labels[iLabel, , ][[1]]
      curr_labels <- sapply(curr_labels, function(x) x[[1]])  # data is contained in an extra list - remove this extra list to get the vector of names
      # put into a data frame with the appropriate column name
      curr_label_column <- eval(parse(text = paste("curr_label_column <- data.frame(", curr_label_name, " = curr_labels)", 
                                                   sep = "")))
      # add to the binned_data_one_site
      binned_data_one_site <- cbind(binned_data_one_site, curr_label_column)
    }
    binned_data <- rbind(binned_data, binned_data_one_site)
  }  # end loop over sites
  
  # convert to a data frame before saving the binned_site_info
  binned_site_info <- data.frame(binned_site_info)
  # Convert all dots in variable names to underscores
  names(binned_site_info) <- gsub(x = names(binned_site_info), pattern = "\\.", replacement = "_")
  names(binned_data) <- gsub(x = names(binned_data), pattern = "\\.", replacement = "_")
  saved_binned_data_file_name <- paste(save_prefix_name, "_", "binned_data_", bin_width, "ms_bins_", sampling_interval, 
                                       "ms_sampled.Rda", sep = "")
  save("binned_data", "binned_site_info", file = saved_binned_data_file_name)
}  # end function

# Examples of how to run the above code to create binned data ...

# raster_directory_name <- '../data/Zhang_Desimone_7objects_raster_data/' bin_width <- 150 sampling_interval <- 10
# save_prefix_name = '../data/ZD'

# Rprof(tmp <- tempfile(), line.profiling=TRUE) create_binned_data_from_matlab_raster_data(raster_directory_name,
# save_prefix_name, bin_width, sampling_interval) Rprof() summaryRprof(tmp, lines = 'show')