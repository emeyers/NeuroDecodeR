# Example of how the code is used...

# # directories of where the .mat raster files are, # and where the .rda files files should be saved basedir_name <-
# '../data/Zhang_Desimone_7objects_matlab_raster_data/' savedir_name <- '../data/Zhang_Desimone_7objects_R_raster_data/'
# file_names <- list.files(basedir_name) # if the directory doesn't exist, create it if (!dir.exists(savedir_name))
# dir.create(savedir_name) # go through each .mat file and convert it into .rda format for (i in seq_along(file_names)){
# print(i) raster_data <- load_matlab_raster_data(paste0(basedir_name, file_names[i])) save_name <- paste0(savedir_name,
# stringr::str_replace(file_names[i], 'mat', 'rda')) save(raster_data, file = save_name, compress = TRUE) }

# The current version of R.matlab on CRAN doesn't not work with R version 3.2.2 can install the package using the
# following command: 

# source('http://callr.org/install#HenrikBengtsson/R.matlab@develop')
# library(R.matlab)

create_raster_data_from_matlab_raster_data <- function(matlab_raster_directory_name){
  # if the directory name ends with a slash, remove this slash
  non_desired_pattern = '.*/$'
  if (grepl(non_desired_pattern, matlab_raster_directory_name) == TRUE){
    matlab_raster_directory_name <- substr(matlab_raster_directory_name, 1, nchar(matlab_raster_directory_name)-1)
  }  
  
  r_raster_directory_name <- paste0(matlab_raster_directory_name, "_rda/")
  
  if(dir.exists(r_raster_directory_name) == FALSE){
    dir.create(r_raster_directory_name)
    
  }
  
  
  matlab_file_names <- list.files(matlab_raster_directory_name)
  for (iSite in 1:length(matlab_file_names)) {
    print(iSite)
    
    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]
    
    # replace .mat with .Rda for matlab_raster_directory_name
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name)-3), "Rda")
    
    raster <- readMat(paste0(matlab_raster_directory_name, "/", curr_matlab_file_name))
    
    
    
    
    # second, create the raster_site_info df
    # parse the raster site info (perhaps there is a better way to do this, but works)
    temp_raster_site_info <- data.frame(raster$raster.site.info)
    temp_raster_site_info_names <- convert_dot_back_to_underscore(row.names(temp_raster_site_info))
    
    raster_site_info <- NULL

    # parse the raster_raster_site_info names if they exist...
    if (length(temp_raster_site_info_names) > 0) {
      for (iSiteInfo in 1:length(temp_raster_site_info_names)) {
        curr_info_data <- unlist(temp_raster_site_info[iSiteInfo, ])
        eval(parse(text = (paste0("raster_site_info$site_info.", eval(temp_raster_site_info_names[iSiteInfo]), " <- curr_info_data"))))
      }
    }
    
    raster_site_info <- data.frame(raster_site_info)
    
    # because list is crazy
    rownames(raster_site_info) <- c()
    
    
    
    
    # third, create the raster_data df
    # Get the raster data
    raster_data <- data.frame(raster$raster.data)
    # Add column names to the raster data in the form of: time.1, time.2 etc.
    data_times <- 1:dim(raster_data)[2]
    
    # (if there is an alignment time, subtract it from the raster times...)
    if (sum(names(raster_site_info) == "site_info.alignment_event_time")) {
      data_times <- (data_times - raster_site_info$site_info.alignment_event_time)
    }
    
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
    
    save(raster_site_info, raster_data, file = paste0(r_raster_directory_name, curr_r_file_name), compress = TRUE)
    
    
    
  }
}

convert_dot_back_to_underscore <- function(oldnames){
  newnames = gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
}
