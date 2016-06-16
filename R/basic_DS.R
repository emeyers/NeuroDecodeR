

# Going to use Winston's R6 package for my OOP rather than the default RC
#  https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
library('R6')    
library('dplyr')
library('reshape2')
library('stringr')

source('helper_functions.R')


basic_DS <- R6Class("basic_DS", 
  
  public = list(
  
    # properties
    binned.data = NA,
    specific.binned.label.name = NA, 
    num.cv.splits = NA,
    use.count.data = FALSE,
  
    
    # constructor
    initialize = function(binned.file.name, specific.binned.label.name, num.cv.splits, use.count.data = FALSE) {
      
      self$specific.binned.label.name <- specific.binned.label.name
      self$num.cv.splits <- num.cv.splits
      
      # load the binned data  
      load(binned.file.name)
  
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
      num.cv.splits = self$num.cv.splits
      
    
  
      # remove all labels that aren't being used, and rename the labels that are being used "labels"
      label.col.ind <- match(paste0("labels.", specific.binned.label.name), names(binned.data))

      binned.data <- binned.data %>% select(siteID, starts_with("time"), labels = label.col.ind)  
      
      
      # order data by: repetitions, sites, labels
      all.k.fold.data <- binned.data  %>% group_by(labels, siteID) %>% sample_n(size = num.cv.splits)
      
      
      unique.labels <- unique(all.k.fold.data$labels)
      num.sites <- length(unique(binned.data$siteID))  
      num.time.bins <- sum(grepl("time.*", names(binned.data)))
      num.labels <- length(unique.labels)
      
      
      # modify a few names in the data
      CV.num <- rep(1:num.cv.splits, num.labels * num.sites)
      
      # add the number of the cross-validitation split to the data ...
      all.k.fold.data2 <- all.k.fold.data
      all.k.fold.data2$CV.num <- CV.num
      
      # paste the site.000 in front of the siteID so that is is listed as site.0001, site.0002, etc
      all.k.fold.data2$siteID <- paste0("site.", str_pad(all.k.fold.data2$siteID, 4, pad = "0"))

      
      
      # reshape the data so that it's [label*time*cv x site]  data frame 
      # can do this quickly using the reshape2 package!
      
      melted.data <- melt(all.k.fold.data2, id.vars = c("siteID", "labels", "CV.num"), 
                          variable.name = "time", value.name = "activity")
      
      all.cv.data <- dcast(melted.data, labels + time + CV.num ~ siteID, value.var = "activity")
      


      # should change this so can put repeats of labels in a CV split...
      
      # create different CV.1, CV.2 which list which points are training points and which points are test points
      for (iCV in 1:num.cv.splits) {
        eval(parse(text=paste0("all.cv.data$CV.", iCV, "= ifelse(all.cv.data$CV.num == iCV, 'test', 'train')")))
      }
      all.cv.data <- select(all.cv.data, -CV.num)  # remove the original CV.num field     

      

      return(all.cv.data)
      
      
      
    }  # end get_data() 
    
    
  )  # end public data/methods
  
  
  
)  # end for the class




