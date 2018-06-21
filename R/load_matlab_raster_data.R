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
library(R.matlab)

load_matlab_raster_data <- function(matlab_file_name) {
  data <- readMat(matlab_file_name)
  # parse the raster site info (perhaps there is a better way to do this, but works)
  temp_site_info <- data.frame(data$raster.site.info)
  temp_site_info_names <- row.names(temp_site_info)
  n_trials <- dim(data$raster.data)[1]
  site_info <- NULL
  
  # parse the raster_site_info names if they exist...
  if (length(temp_site_info_names) > 0) {
    for (iSiteInfo in 1:length(temp_site_info_names)) {
      curr_info_data <- unlist(temp_site_info[iSiteInfo, ])
      curr_info_data <- rep(curr_info_data, n_trials)
      eval(parse(text = (paste0("site_info$site_info_", eval(temp_site_info_names[iSiteInfo]), " <- curr_info_data"))))
    }
  }
  
  site_info <- data.frame(site_info)
  # Get the raster data
  raster_data <- data.frame(data$raster.data)
  # Add column names to the raster data in the form of: time.1, time.2 etc.
  data_times <- 1:dim(raster_data)[2]
  
  # (if there is an alignment time, subtract it from the raster times...)
  if (sum(names(site_info) == "alignment.event.time")) {
    data_times <- (data_times - site_info$alignment.event.time)
  }
  
  names(raster_data) <- paste0("time_", data_times)
  # Get the labels for what happened on each trial and add them to the raster.data data frame
  raster_labels <- data$raster.labels
  # loop over label names
  label_names <- row.names(raster_labels)
  
  for (iLabel in 1:length(label_names)) {
    # get the name for the current raster_labels
    curr_label_name <- eval(label_names[iLabel])
    # add the prefix labels. to the curr label name...
    curr_label_name <- paste("labels_", curr_label_name, sep = "")
    # extract the labels themselves...
    curr_labels <- raster_labels[iLabel, , ][[1]]
    curr_labels <- sapply(curr_labels, function(x) x[[1]])  # data is contained in an extra list - remove this extra list to get the vector of names
    # put into a data frame with the appropriate column name
    curr_label_column <- eval(parse(text = paste("curr_label_column <- data.frame(", curr_label_name, " = curr_labels)", 
                                                 sep = "")))
    # add to the raster.data raster_data <- cbind(raster_data, curr_label_column)
    raster_data <- cbind(curr_label.column, raster_data)
  }
  
  # add the raster site info as attributes to the data frame… if the site.info is empty, don't do anything
  if (dim(site_info)[1] != 0) {
    raster_data <- cbind(site_info, raster_data)
  }
  
  # Replace all dots in variable names to underscore
  names(raster_data) <- gsub(x = names(raster_data), pattern = "\\.", replacement = "_")
  
  return(raster_data)
}