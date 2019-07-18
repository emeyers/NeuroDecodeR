#' A function that converts raster data in MATLAB .mat format to R .rda format
#' 
#' 
#' 
#' ! link to raster format and related two functions
#'
#' @export

convert_matlab_raster_data <- function(matlab_raster_dir_name, 
                                       r_raster_dir_name = NULL, 
                                       start_ind = NULL, 
                                       end_ind = NULL,
                                       files_contain = ""){
  
  
  
  # if matlab directory name ends with a slash, remove this slash (should work for all OS)
  matlab_raster_dir_name <- paste0(file.path(dirname(matlab_raster_dir_name), ''), 
                                   basename(matlab_raster_dir_name))  
  

  matlab_file_names <- list.files(matlab_raster_dir_name, pattern = files_contain)
  
  
  
  for (iSite in seq_along(matlab_file_names)) {
  
    cat(paste(iSite, " "))
    
    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]
    
    # replace .mat with .rda for matlab_raster_directory_name
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name)-3), "rda")
    
    raster <- R.matlab::readMat(paste0(matlab_raster_dir_name, "/", curr_matlab_file_name))
    
    
    
    # second, create the raster_site_info list
    raster_site_info <- raster$raster.site.info[, , 1]
    raster_site_info_names <- convert_dot_back_to_underscore(names(raster_site_info))
    names(raster_site_info) <- raster_site_info_names
    
    # take elements out from a matrix into an ordinary list
    raster_site_info <- lapply(raster_site_info, function(x) x[[1]])
    
    # in the future, might consider making site_info.X, site_info.Y etc,
    # variables in the raster_data and not having a separate object
    # raster_site_info
    
    
    # third, create the raster_data df
    raster_data <- data.frame(raster$raster.data)
    
    
    # if start or end inds are not specified used the full data set
    # also create save names if start and end inds are specified 
    if (is.null(start_ind)) {
      start_ind <- 1
      start_ind_save_dir_name <- ""
    } else {
      start_ind_save_dir_name <- paste0("_start_", start_ind)
    }
      
      
    if (is.null(end_ind)) {
      end_ind <- dim(raster_data)[2]
      end_ind_save_dir_name <- ""
    } else {
      end_ind_save_dir_name <- paste0("_end_", end_ind)
    }
    
    
    raster_data <- raster_data[, start_ind:end_ind]

    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_times <- 1:dim(raster_data)[2]
    
    # if there is an alignment time, subtract the start_ind offset from the
    # alignment and subtract alignment from the raster times
    if (sum(names(raster_site_info) == "alignment_event_time")) {
      
      data_times <- (data_times - rep.int(raster_site_info$alignment_event_time - (start_ind - 1), length(data_times)))

      # update the names if start_ind or end_ind were given as arguments      
      if (!(start_ind_save_dir_name == ""))
        start_ind_save_dir_name <- paste0("_start_", start_ind - raster_site_info$alignment_event_time)
      
      if (!(end_ind_save_dir_name == ""))
        end_ind_save_dir_name <- paste0("_end_", end_ind - raster_site_info$alignment_event_time)
    } 
    
    names(raster_data) <- paste0("time.", data_times)
    
    
    
    # forth, add the labels to raster_data
    # Get the labels for what happened on each trial and add them to the raster_data data frame
    raster_labels <- raster$raster.labels
    
    
    # loop over label names
    all_labels <- convert_dot_back_to_underscore(row.names(raster_labels))
    
    for (iLabel in seq_along(all_labels)) {
      
      # get the name for the current raster_labels
      curr_var_name <- all_labels[iLabel]
      
      # add the prefix labels. to the curr label name...
      curr_var_name <- paste0("labels.", curr_var_name)
      
      # levels are contained in an extra list - remove this extra list to get the vector of names
      curr_levels <- raster_labels[iLabel, , ][[1]]
      curr_levels <- sapply(curr_levels, function(x) x[[1]])
      
      # put into a data frame with the appropriate column name
      curr_var_column <- eval(parse(text = paste0("curr_var_column <- data.frame(", curr_var_name, " = curr_levels)")))
      
      # add to the raster_data 
      raster_data <- cbind(curr_var_column, raster_data)
      
    }
    
    
    # finally, save both raster_site_info and raster data in the file
    if(is.null(r_raster_dir_name)) {
      
      # if the directory name ends with "_mat", remove "_mat"
      non_desired_pattern = '.*_mat$'
      if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE){
        r_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 4)
      } 
      
      # append start and end index if applicable and append "_rda/"
      r_raster_dir_name <- paste0(r_raster_dir_name, start_ind_save_dir_name, end_ind_save_dir_name, "_rda/")
      
    }
    
    
    if(dir.exists(r_raster_dir_name) == FALSE){
      dir.create(r_raster_dir_name)
    }
    
    
    save(raster_site_info, raster_data, file = paste0(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    

  }
  
  r_raster_dir_name
  
}



# a helper function to convert . to _ 
convert_dot_back_to_underscore <- function(oldnames) {
  newnames = gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
  
}


  