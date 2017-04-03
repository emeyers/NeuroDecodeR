




# Example of how the code is used...


# # directories of where the .mat raster files are, 
# # and where the .rda files files should be saved
# basedir_name <- '../data/Zhang_Desimone_7objects_matlab_raster_data/'
# savedir_name <- '../data/Zhang_Desimone_7objects_R_raster_data/'
# file_names <- list.files(basedir_name)
#  
# 
# # if the directory doesn't exist, create it
# if (!dir.exists(savedir_name))
#   dir.create(savedir_name)
# 
# 
# # go through each .mat file and convert it into .rda format 
# for (i in seq_along(file_names)){
# 
#   print(i)
#   
#   raster_data <- load_matlab_raster_data(paste0(basedir_name, file_names[i]))
# 
#   save_name <- paste0(savedir_name, stringr::str_replace(file_names[i], 'mat', 'rda'))
# 
#   save(raster_data, file = save_name, compress = TRUE)
# 
# }
# 





# The current version of R.matlab on CRAN doesn't not work with R version 3.2.2
# can install the package using the following command: 

# source('http://callr.org/install#HenrikBengtsson/R.matlab@develop')


library(R.matlab)


load_matlab_raster_data <- function(matlab_file_name) {
  
  
  data <- readMat(matlab_file_name)  
  
  
  # parse the raster site info (perhaps there is a better way to do this, but works)
  temp.site.info <- data.frame(data$raster.site.info)
  temp.site.info.names <- row.names(temp.site.info)
  
  n_trials <- dim(data$raster.data)[1]
  
  site.info <- NULL
  # parse the raster_site_info names if they exist...
  if (length(temp.site.info.names) > 0){  
    
     for (iSiteInfo in 1:length(temp.site.info.names)) {
       curr.info.data <- unlist(temp.site.info[iSiteInfo, ])
       curr.info.data <- rep(curr.info.data, n_trials)
       eval(parse(text = (paste0("site.info$site.info.", eval(temp.site.info.names[iSiteInfo]), " <- curr.info.data"))))
     }
  }
  
  
  
  site.info <- data.frame(site.info)
  
  # Get the raster data
  raster.data <- data.frame(data$raster.data)
  
  
  # Add column names to the raster data in the form of: time.1, time.2 etc.
  data.times <- 1:dim(raster.data)[2]
  
  # (if there is an alignment time, subtract it from the raster times...) 
  if (sum(names(site.info) == "alignment.event.time")) {
    data.times <- (data.times - site.info$alignment.event.time)
  }
  
  names(raster.data) <- paste0("time.", data.times)
  
  
  
  
  # Get the labels for what happened on each trial and add them to the raster.data data frame
  raster.labels <- data$raster.labels
  
  # loop over label names
  label.names <- row.names(raster.labels)
  for (iLabel in 1:length(label.names)){
    
    # get the name for the current raster_labels
    curr.label.name <- eval(label.names[iLabel])
    
    # add the prefix labels. to the curr label name...
    curr.label.name <- paste('labels.', curr.label.name, sep = "")
    
    # extract the labels themselves...
    curr.labels <- raster.labels[iLabel, ,][[1]]
    curr.labels <- sapply(curr.labels, function(x) x[[1]])    # data is contained in an extra list - remove this extra list to get the vector of names
    
    # put into a data frame with the appropriate column name
    curr.label.column <- eval(parse(text = paste('curr.label.column <- data.frame(', curr.label.name, ' = curr.labels)', sep ="")))
    
    # add to the raster.data
    # raster.data <- cbind(raster.data, curr.label.column)
    raster.data <- cbind(curr.label.column, raster.data)
    
  }
  
  

  
  # add the raster site info as attributes to the data frame...
  if (dim(site.info)[1] != 0)   # if the site.info is empty, don't do anything
    raster.data <- cbind(site.info, raster.data)
  

  return(raster.data)
  
}





















