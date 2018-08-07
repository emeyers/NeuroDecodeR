#' A function that converts raster data in .mat format to .Rda format
#' 
#' ! link to raster format and related two functions
#'
#' @usage \code{create_raster_data_from_matlab_raster_data(matlab_raster_dir_name, r_raster_dir_name = NULL)}
#' 
#' @param matlab_raster_dir_name character. Name of a directory containing raster data in .mat format.
#' @param r_raster_dir_name character. Name of the directory to store created raster data in .Rda format.
#' By default, it is created by removing the "_mat" suffix of \code{matlab_raster_dir_name} if applicable
#' and appending '_rda' to it.
#' @return Preceding the creation of each raster file, it spills the total number of raster files will have been created as you will see
#' the number increments by one. After writing all raster files, it spills the \code{r_raster_dir_name}.
#' @example
#' \dontrun{
#' create_raster_data_from_matlab_raster_data(file.path(getwd(), "data/raster/Zhang_Desimone_7objects_raster_data_mat"))
#' }
#' @import R.matlab
#' @export

create_raster_data_from_matlab_raster_data <- function(matlab_raster_dir_name, r_raster_dir_name = NULL){
  
  # zero, create destination dir
  if(is.null(r_raster_dir_name)){
    # if the directory name ends with a slash, remove this slash
    non_desired_pattern = '.*/$'
    if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE){
      matlab_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 1)
    }  
    
    # if the directory name ends with "_mat", remove "_mat"
    non_desired_pattern = '.*_mat$'
    if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE){
      r_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 4)
    } 
    
    # append "_rda/"
    r_raster_dir_name <- paste0(r_raster_dir_name, "_rda/")
    
    
  }
  
  if(dir.exists(r_raster_dir_name) == FALSE){
    dir.create(r_raster_dir_name)
    
  }

  
  matlab_file_names <- list.files(matlab_raster_dir_name)
  for (iSite in 1:length(matlab_file_names)) {
    print(iSite)
    
    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]
    
    # replace .mat with .Rda for matlab_raster_directory_name
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name)-3), "Rda")
    
    raster <- readMat(paste0(matlab_raster_dir_name, "/", curr_matlab_file_name))
    
    
    
    # second, create the raster_site_inf0 list
    
    raster_site_info <- raster$raster.site.info[,,1]
    

    
    
    
    
    # third, create the raster_data df
    # Get the raster data
    raster_data <- data.frame(raster$raster.data)
    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_times <- 1:dim(raster_data)[2]
    
    # abandoned for over-engineering
    # # (if there is an alignment time, subtract it from the raster times...)
    # if (sum(names(raster_site_info) == "alignment_event_time")) {
    #   data_times <- (data_times - raster_site_info$alignment_event_time)
    # }
    
    names(raster_data) <- paste0("time.", data_times)
    
    
    
    # forth, add the labels to raster_data
    # Get the labels for what happened on each trial and add them to the raster.data data frame
    raster_labels <- raster$raster.labels
    # loop over label names
    label_names <- convert_dot_back_to_underscore(row.names(raster_labels))
    
    for (iLabel in 1:length(label_names)) {
      # get the name for the current raster_labels
      curr_label_name <- label_names[iLabel]
      # add the prefix labels. to the curr label name...
      curr_label_name <- paste0("labels.", curr_label_name)
      # extract the labels themselves...
      curr_labels <- raster_labels[iLabel, , ][[1]]
      curr_labels <- sapply(curr_labels, function(x) x[[1]])  # data is contained in an extra list - remove this extra list to get the vector of names
      # put into a data frame with the appropriate column name
      curr_label_column <- eval(parse(text = paste0("curr_label_column <- data.frame(", curr_label_name, " = curr_labels)")))
      # add to the raster.data raster_data <- cbind(raster_data, curr_label_column)
      raster_data <- cbind(curr_label_column, raster_data)
    }
    
    
    
    
    # finally, save both raster_site_info and raster data in the file
    
    save(raster_site_info, raster_data, file = paste0(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    
    
  }
  
  print(r_raster_dir_name)
  
}

convert_dot_back_to_underscore <- function(oldnames){
  newnames = gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
}
