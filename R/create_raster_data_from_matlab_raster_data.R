#' A function that converts raster data in .mat format to .Rda format
#' 
#' ! link to raster format and related two functions
#' If the data was sampled at 1000 Hz and there will be no need to compute time index from sample index. Otherwise, you must provide sample duration in \code{raster_site_info.timing_info.sampling _period_in_ms}
#' 
#' @usage \code{create_raster_data_from_matlab_raster_data(matlab_raster_dir_name, r_raster_dir_name = NULL)}
#' 
#' @param matlab_raster_dir_name character. Name of a directory containing raster data in .mat format.
#' @param r_raster_dir_name character. Name of the directory to store created raster data in .Rda format.
#' By default, it is created by removing the "_mat" suffix of \code{matlab_raster_dir_name} if applicable
#' and appending '_rda' to it.
#' @param start_time integer. It specifies the time where the new raster data begin. By default, all data are included.
#' @param end_time integer. It specifies the time where the new raster data end. By default, all data are included.
#' @param files_contain regular expression. Only raster data files that match the file_contains are inlcluded. By default, it is an empty character.
#' @return Directory for new raster data will be created, and new raster data files will be written under it. During execution, preceding the creation of each raster file, console spills the total number of raster files will have been created (as you will see
#' the number increments by one). After writing all raster files, console spills the \code{r_raster_dir_name}.
#' @examples
#' \dontrun{
#' create_raster_data_from_matlab_raster_data(file.path(getwd(), "data/raster/Zhang_Desimone_7objects_raster_data_mat"))
#' }
#' If you get other files mixed in the raster directory that are not .mat files and only want to include data from 200th sample to 800th sample
#' \dontrun{
#' create_raster_data_from_matlab_raster_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_mat/'), start_time= -400, end_time=500, files_contain="\\.mat$")
#' }
#' @export

create_raster_data_from_matlab_raster_data <- function(matlab_raster_dir_name, r_raster_dir_name = NULL, start_time = NULL, end_time = NULL,
                                                       files_contain = ""){
  # if matlab directory name ends with a slash, remove this slash
  
  non_desired_pattern = '.*/$'
  if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE){
    matlab_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 1)
  }  
  
  
  
  
  matlab_file_names <- list.files(matlab_raster_dir_name, pattern = files_contain)
  for (iSite in 1:length(matlab_file_names)) {
    cat(paste(iSite, " "))
    
    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]
    
    # replace .mat with .Rda for matlab_raster_directory_name
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name)-3), "Rda")
    
    raster <- R.matlab::readMat(paste0(matlab_raster_dir_name, "/", curr_matlab_file_name))
    
    
    
    # second, create the raster_site_info list
    
    raster_site_info <- raster$raster.site.info[,,1]
    timing_info <- raster_site_info$timing.info[,,1]
    
    
    
    # third, create the raster_data df
    # Get the raster data
    raster_data <- data.frame(raster$raster.data)
    
    
    
    if (is.null(start_time)) {
      start_ind <- 1
      bStart_time <- FALSE
    }else {
      bStart_time <- TRUE
    } 
    
    if (is.null(end_time)) {
      end_ind <- dim(raster_data)[2]
      bEnd_time <- FALSE
      
    } else {
      bEnd_time <- TRUE
    }
    
    
    
    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_samples <- 1:dim(raster_data)[2]
    
    
    # if there is an alignment time, subtract the start_time offset from the alignment and subtract alignment from the raster times; also, subtract alignment fron start_time ot get new start_time
    # 
    
    
    if (sum(names(timing_info) == "stimulus.onset.sampling.index")) {
      data_samples <- (data_samples - rep.int(timing_info$stimulus.onset.sampling.index, length(data_samples)))
      # start_time_new <- start_time - timing_info$stimulus.onset.sampling.index
      # 
      # end_time_new <- end_time - timing_info$stimulus.onset.sampling.index
      # 
      # 
    }
    
    if (sum(names(timing_info) == "sampling.frequency.in.hz")){
      if (timing_info$sampling.frequency.in.hz == 1000){
        data_start_times <- data_samples
        data_end_times <- data_samples
        
        names(raster_data) <- paste0("time.", data_times)
        
      } else {
        data_start_times <- round(data_samples * as.vector(1000 / timing_info$sampling.frequency.in.hz))
        data_end_times <- round((data_samples + as.vector(1)) * as.vector(1000 / timing_info$sampling.frequency.in.hz))
        names(raster_data) <- paste0("time.", data_start_times, "_", data_end_times)
        
      }
    } else {
      stop("To create raster data in .Rda format, you must have a field called sampling_frequency_in_hz in the struct raster_site_info")
    }
    

    
    
    if(bStart_time){
      start_ind <- 1 + sum(data_start_times < start_time)
    }
    if(bEnd_time){
      end_ind <- sum(data_end_times < end_time)
    }
    
    raster_data <- raster_data[,start_ind:end_ind]
      
    
    # forth, add the labels to raster_data
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
      raster_data <- cbind(curr_var_column, raster_data)
    }
    
    
    # finally, save both raster_site_info and raster data in the file
    if(is.null(r_raster_dir_name)){
      
      # if the directory name ends with "_mat", remove "_mat"
      non_desired_pattern = '.*_mat$'
      if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE){
        r_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 4)
      } 
      
      # append start and end index if applicable and append "_rda/"
      
      
      start_time_name <- ""
      end_time_name <- ""
      
      if (bStart_time) {
        start_time_name <- paste0("_start_", start_time, "ms")
      } 
      if (bEnd_time) {
        end_time_name <- paste0("_end_", end_time, "ms")
      }
      r_raster_dir_name <- paste0(r_raster_dir_name, start_time_name, end_time_name, "_rda/")
      
      
    }
    
    if(dir.exists(r_raster_dir_name) == FALSE){
      dir.create(r_raster_dir_name)
      
    }
    
    
    save(raster_site_info, raster_data, file = paste0(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    
    
  }
  
  print(r_raster_dir_name)
  
}

convert_dot_back_to_underscore <- function(oldnames){
  newnames = gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
  
}


