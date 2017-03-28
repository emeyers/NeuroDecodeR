

# # clear all the variables in memory before running
# # rm(list = ls())




create_binned_data_from_matlab_raster_data <- function(raster.directory.name, save_prefix_name, bin.width, sampling.interval)  # not implemented yet: [start_time], [end_time])
{

  
  require('R.matlab')
  require('tidyr')
  require('dplyr')

  # helper funciton: every list in raster.labels is contained in an extra list 
  # the function below removes this extra list so that it will just return the items of interest
  delist <- function(data.in.list) sapply(data.in.list, function(x) x[[1]])
  
  
  file.names <- list.files(raster.directory.name)
  
  binned.site.info <- NULL
  binned.data <- NULL
  
  
  for (siteID in 1:length(file.names)) {
    
    
    
    print(siteID)
    
    # current file name
    curr.matlab.file.name <- file.names[siteID]
    
    raster.data.list.imported.from.matlab <- readMat(paste(raster.directory.name, curr.matlab.file.name, sep = "")) 
    
    
    
    # parse the raster site info...
    raster.site.info <- data.frame(raster.data.list.imported.from.matlab$raster.site.info)
    raster.site.info.names <- row.names(raster.site.info)
  
      
    for (iSiteInfo in 1:length(raster.site.info.names))
    {
      curr.info.data <- unlist(raster.site.info[iSiteInfo, ])
      eval(parse(text = (paste0("binned.site.info$", eval(raster.site.info.names[iSiteInfo]), "[", eval(siteID) , "] <- curr.info.data"))))
    }
    
  
  
    # parse the raster data
    
    raster.data <- data.frame(raster.data.list.imported.from.matlab$raster.data)
    
    # start by smoothing the data to create the binned data with a boxcar filter
    the.filter <- rep(1, bin.width)/bin.width
    
    # use apply to get out smoothed data
    binned.data.one.site <- data.frame(t(apply(raster.data, 1, stats::filter, the.filter)))
    
    
    # down-sample the data now to save on memory...
    start.ind <- max(which(is.na(binned.data.one.site[1, 1:bin.width]))) + 1
    end.ind <- min(which(is.na(binned.data.one.site[1, bin.width:ncol(binned.data.one.site)]))) + bin.width - 1
    sampling.inds <- seq(start.ind, end.ind, by = sampling.interval)
    binned.data.one.site <- binned.data.one.site[, sampling.inds]
    

    # alternatively - rather than listing the whole interval, can just list the center of the bin...
    # library('stringr')
    # names(binned.data.one.site) <- str_replace(names(binned.data.one.site), "X", "time.")
        

    # create better labels for the columns specifying the time bins
    if (sum(raster.site.info.names == "alignment.event.time") > 0){
      alignment.event.time <- as.numeric(raster.site.info["alignment.event.time", ])
    } else {
      alignment.event.time <- 0
    }

    start.time.labels <- (sampling.inds - bin.width/2 + 1) - alignment.event.time
    end.time.labels <- (sampling.inds + bin.width/2) - alignment.event.time
    
    names(binned.data.one.site) <- paste0("time.", start.time.labels, ".", end.time.labels)
    

    
    # can parse the times later using:  unlist(strsplit(x, ".", fixed = TRUE))
    
    
    # can plot things to show it worked (just need to make screen larger)
    #plot(apply(binned.data.one.site, 2, mean, na.rm = TRUE), type = 'l')
 
    
    # add in the site's ID
    siteID.df <- data.frame(siteID = rep(siteID, dim(binned.data.one.site)[1]))
    binned.data.one.site <- cbind(siteID = siteID.df, binned.data.one.site)
    
    
    
    
    # add labels...
    raster.labels <- raster.data.list.imported.from.matlab$raster.labels
    
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
      
      # add to the binned.data.one.site
      binned.data.one.site <- cbind(binned.data.one.site, curr.label.column)
      
    }
    
  
    binned.data <- rbind(binned.data, binned.data.one.site)
    
  }  # end loop over sites
  

  # convert to a data frame before saving the binned.site.info
  binned.site.info <- data.frame(binned.site.info)
  
  
  saved_binned_data_file_name <- paste(save_prefix_name, "_", "binned_data_", bin.width, "ms_bins_", sampling.interval, "ms_sampled.Rda", sep = "")
  save("binned.data", "binned.site.info", file = saved_binned_data_file_name)
  
}   # end function




# Examples of how to run the above code to create binned data ...

# raster.directory.name <- "../data/Zhang_Desimone_7objects_raster_data/"
# bin.width <- 150
# sampling.interval <- 10
# save_prefix_name = "../data/ZD"

# Rprof(tmp <- tempfile(), line.profiling=TRUE)
# create_binned_data_from_matlab_raster_data(raster.directory.name, save_prefix_name, bin.width, sampling.interval)
# Rprof()
# summaryRprof(tmp, lines = "show")












