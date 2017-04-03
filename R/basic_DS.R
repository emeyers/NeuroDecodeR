#' A basic datasource object
#'
#' A datasource object takes data in binned format and returns training 
#' and testing splits of the data that can be passed to a classifier. 
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section basic_DS constructor:
#' 
#' \describe{
#' \item{\code{basic_DS$new(binned.data, specific.binned.label.name, num.cv.splits, use.count.data, num.times.to.repeat.labels.per.cv.block )}}{
#' if successful, will return a new \code{basic_DS} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{get_data}}{
#' This method returns a data frame that has the training and test splits of the data.
#' }}
#' 
#' 
#' 
#' @import R6
#' @export





basic_DS <- R6Class("basic_DS", 
  
  public = list(
  
    # properties
    binned.data = NA,
    specific.binned.label.name = NA, 
    num.cv.splits = NA,
    use.count.data = FALSE,
    num.times.to.repeat.labels.per.cv.block = 1,
  
    
    # constructor
    initialize = function(binned.file.name, specific.binned.label.name, num.cv.splits, use.count.data = FALSE) {
      
      self$specific.binned.label.name <- specific.binned.label.name
      self$num.cv.splits <- num.cv.splits
      
      # load the binned data  
      load(binned.file.name)
      
      # now called binned.data binned_data, should really refactor my code to my this change throughout 
      if (!exists("binned.data"))
        binned.data <- binned_data
      
  
      if (use.count.data) {
        binned.data <- convert.rates.to.counts(binned.data) 
      }
    
      self$binned.data <- binned.data
      
    },
  
    

    # methods
    get_data = function(){
      
      
      # defining these here to make it potentially easy to transfer my code to other R OO systems 
      # (at the cost of a little memory)
      binned.data <- self$binned.data 
      specific.binned.label.name <- self$specific.binned.label.name
      num.trials.used.per.label <- self$num.cv.splits * self$num.times.to.repeat.labels.per.cv.block 
    
  
      # remove all labels that aren't being used, and rename the labels that are being used "labels"
      label.col.ind <- match(paste0("labels.", specific.binned.label.name), names(binned.data))

      binned.data <- binned.data %>% select(siteID, starts_with("time"), labels = label.col.ind)  
      
      
      # order data by: repetitions, sites, labels
      all.k.fold.data <- binned.data  %>% group_by(labels, siteID) %>% sample_n(size = num.trials.used.per.label)
      
      
      unique.labels <- unique(all.k.fold.data$labels)
      num.sites <- length(unique(binned.data$siteID))  
      num.time.bins <- sum(grepl("time.*", names(binned.data)))
      num.labels <- length(unique.labels)
      
      
      # add a few names in the data frame
      
      # CV.slice.ID is a groups of data that have one example for each label
      #  - these groups are mapped into CV blocks where blocks contain num.times.to.repeat.labels.per.cv.block of each label  
      CV.slice.ID <- rep(1:num.trials.used.per.label, num.labels * num.sites)
      
      # add the number of the cross-validitation split to the data ...
      all.k.fold.data$CV.slice.ID <- CV.slice.ID
      
      # paste the site.000 in front of the siteID so that is is listed as site.0001, site.0002, etc
      all.k.fold.data$siteID <- paste0("site.", stringr::str_pad(all.k.fold.data$siteID, 4, pad = "0"))

      
      # reshape the data so that it's [label*time*cv x site]  data frame 
      # can do this quickly using the reshape2 package!
      
      melted.data <- reshape2::melt(all.k.fold.data, id.vars = c("siteID", "labels", "CV.slice.ID"), 
                          variable.name = "time", value.name = "activity")
      
      all.cv.data <- reshape2::dcast(melted.data, labels + time + CV.slice.ID ~ siteID, value.var = "activity")
      


      # below I changed this so can put repeats of labels in a CV split...
      # # create different CV.1, CV.2 which list which points are training points and which points are test points
      # for (iCV in 1:num.cv.splits) {
      #   eval(parse(text=paste0("all.cv.data$CV.", iCV, "= ifelse(all.cv.data$CV.slice.ID == iCV, 'test', 'train')")))
      # }
      # all.cv.data <- select(all.cv.data, -CV.slice.ID)  # remove the original CV.slice.ID field     

      
      
      # create different CV.1, CV.2 which list which points are training points and which points are test points
      for (iCV in 1:self$num.cv.splits) {
        start.ind <- (((iCV - 1) * self$num.times.to.repeat.labels.per.cv.block) + 1)
        end.ind <- (iCV * self$num.times.to.repeat.labels.per.cv.block)
        curr.cv.block.inds <- start.ind:end.ind
        eval(parse(text=paste0("all.cv.data$CV.", iCV, "= ifelse(all.cv.data$CV.slice.ID %in% curr.cv.block.inds, 'test', 'train')")))
      }
      all.cv.data <- select(all.cv.data, -CV.slice.ID)  # remove the original CV.slice.ID field     
      
      
      return(all.cv.data)
      
      
      
    }  # end get_data() 
    
    
  )  # end public data/methods
  
  
  
)  # end for the class




