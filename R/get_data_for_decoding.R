

library('dplyr')

# similar to the NDT data sources get_data method...

# should turn this into a full basic_DS S4 object...




get_data_for_decoding <- function(binned.file.name, specific.binned.label.name, num.cv.splits)
{

  

  # load the binned data  
  load(binned.file.name)
  
  
  # remove all labels that aren't being used, and rename the labels that are being used "labels"
  label.col.ind <- match(paste0("labels.", specific.binned.label.name), names(binned.data))
  #binned.data <- binned.data %>% select(siteID, starts_with("X"), labels = label.col.ind)  # should switch the "X" with "Time"
  binned.data <- binned.data %>% select(siteID, starts_with("time"), labels = label.col.ind)  
  
  
  # order data by: repitions (1-20), sites (1-132), stimuli (1-7)
  all.k.fold.data <- binned.data  %>% group_by(labels, siteID) %>% sample_n(size = num.cv.splits)
  
  unique.labels <- unique(all.k.fold.data$labels)
  
  
  num.sites <- length(unique(binned.data$siteID))  
  num.time.bins <- sum(grepl("time.*", names(binned.data)))
  num.labels <- length(unique.labels)
  
  
  all.cv.data <- NULL
  #all.cv.data <- data.frame(matrix(NA, (num.cv.splits * num.labels * num.time.bins), (num.sites + 3)))
  for (iCV in 1:num.cv.splits)
  {

    print(iCV)
    
    # get all the data for the current CV 
    #  the rows will be in the form [site1, site2, ... siten, site1, site2, ..., siten, ...] 
    #  where the repeats in the sites are due to different labels
    inds <- seq(iCV, nrow(all.k.fold.data), by = num.cv.splits)
    curr.cv.data <- all.k.fold.data[inds, ]


    
    # convert to a data frame in the form:  [time-label x sites] that will be useful for a classifier
    for (iLabel in 1:num.labels)
    {
      
#       curr.label.data <- curr.cv.data %>% 
#         filter(labels == unique.labels[iLabel]) %>%
#         select(starts_with("time"))     # this select doesn't appear to be working...
#         #select(starts_with("X"))     # this select doesn't appear to be working...
#       
#       temp.cv.data <- t(curr.label.data)
#       
#       # remove the first row which contains the siteIDs and the label names (shouldn't be there if the select was working...)
#       temp.cv.data <- temp.cv.data[3:nrow(temp.cv.data), ]
#       
      
      # much faster version of the code above...
      curr.ind <- (((iLabel - 1) * num.sites) + 1):(iLabel * num.sites)
      temp.cv.data <- t(curr.cv.data[curr.ind, 2:(num.time.bins + 1)])
      
      
      # get the names of the time periods
      time.names <- row.names(temp.cv.data)
      
      # convert all the values to numeric (for some reason taking the transpose made them factors)
      temp.cv.data <-  data.frame(apply(temp.cv.data, 2, function(x) as.numeric(as.character(x))))


      # add columns specifying: 1)  the time the data came from, 2) the label ID, 3) the CV split
#       temp.cv.data <- temp.cv.data %>% 
#         mutate(time = time.names) %>% 
#         mutate(labels = rep(unique.labels[iLabel], nrow(temp.cv.data)))  %>% 
#         mutate(CV.num = rep(iCV, nrow(temp.cv.data)))
#       
       
      temp.cv.data$time <- time.names
      temp.cv.data$labels <- rep(unique.labels[iLabel], nrow(temp.cv.data))
      temp.cv.data$CV.num <- rep(iCV, nrow(temp.cv.data))
      
      
      
      all.cv.data <- rbind(all.cv.data, temp.cv.data)
      
      #start.ind <- ((iCV - 1) * (num.time.bins * num.labels)) + 1
      #end.ind <- (iCV * num.time.bins * length(unique.labels))
      #all.cv.data[start.ind:end.ind, ] <- temp.cv.data
      names(all.cv.data) <- names(temp.cv.data)
      
    }  # end for loop over the labels
    
    
  }    # end the for loop over CV splits
  

  # turn the labels into factors so the classifier can use them
  all.cv.data$labels <- as.factor(all.cv.data$labels)
  
  return(all.cv.data)
  
}
    
    
