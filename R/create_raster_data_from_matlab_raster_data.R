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
#' @param start_ind integer. It specifies the sample index where the new raster data begin. Due to the structure of raster data in matlab, all sample indices should be positive. By default, all data are included.
#' @param end_ind integer. It specifies the sample index where the new raster data end. Due to the structure of raster data in matlab, all sample indices should be positive. By default, all data are included.
#' @param files_contain regular expression. Only raster data files that match the file_contains are inlcluded. By default, it is an empty character.
#' @return Directory for new raster data will be created, and new raster data files will be written under it. During execution, preceding the creation of each raster file, console spills the total number of raster files will have been created (as you will see
#' the number increments by one). After writing all raster files, console spills the \code{r_raster_dir_name}.
#' @examples
#' \dontrun{
#' create_raster_data_from_matlab_raster_data(file.path(getwd(), "data/raster/Zhang_Desimone_7objects_raster_data_mat"))
#' }
#' If you get other files mixed in the raster directory that are not .mat files and only want to include data from 200th sample to 800th sample
#' \dontrun{
#' create_raster_data_from_matlab_raster_data(file.path(getwd(),'data/raster/Zhang_Desimone_7objects_raster_data_mat/'), start_ind=200, end_ind=800, files_contain="\\.mat$")
#' }
#' @export

create_raster_data_from_matlab_raster_data <- function(matlab_raster_dir_name, r_raster_dir_name = NULL, start_ind = NULL, end_ind = NULL,
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
    
    
    
    
    
    
    # third, create the raster_data df
    # Get the raster data
    raster_data <- data.frame(raster$raster.data)
    

    
    if (is.null(start_ind)) {
      start_ind <- 1
      bStart_ind <- FALSE
}
    
    if (is.null(end_ind)) {
      end_ind <- dim(raster_data)[2]
      bEnd_ind <- FALSE
      
    } 
    
    
    raster_data <- raster_data[,start_ind:end_ind]
    
    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_times <- 1:dim(raster_data)[2]


    # if there is an alignment time, subtract the start_ind offset from the alignment and subtract alignment from the raster times; also, subtract alignment fron start_ind ot get new start_ind
    if (sum(names(raster_site_info) == "alignment.event.time")) {
      data_times <- (data_times - rep.int(raster_site_info$alignment.event.time - (start_ind - 1), length(data_times)))
      start_ind_new <- start_ind - raster_site_info$alignment.event.time
      
      end_ind_new <- end_ind - raster_site_info$alignment.event.time
      
    }
    
    names(raster_data) <- paste0("time.", data_times)
    
    
    
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
      
      
      start_ind_name <- ""
      end_ind_name <- ""
      
      if (bStart_ind) {
        start_ind_name <- paste0("_start_", start_ind_new)
      } 
      if (bEnd_ind) {
        end_ind_name <- paste0("_end_", end_ind_new)
      }
      r_raster_dir_name <- paste0(r_raster_dir_name, start_ind_name, end_ind_name, "_rda/")
      
      
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


  