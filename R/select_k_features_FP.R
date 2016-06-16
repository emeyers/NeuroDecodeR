




select_k_features_FP <- R6Class("select_k_features_FP", 
                    
  public = list(
                      
    
     # properties
     num.site.to.use = NA,
     num.sites.to.exclude = NA,
     
                      
     # constructor
     initialize = function(num.site.to.use, num.sites.to.exclude) {
       
       if (!missing(num.site.to.use)) {
         self$num.site.to.use <- num.site.to.use
       }
       
       if (!missing(num.sites.to.exclude)) {
         self$num.site.to.exclude <- num.site.to.exclude
       }
       
     },
                      
                      
                      
     # methods
     preprocess_data = function(train.data, test.data){
             

       if (is.na(self$num.site.to.use) && is.na(self$num.sites.to.exclude)) {
         stop('Either num.site.to.use or num.site.to.exclude must be set prior to calling the preprocess_data method')
       }
       
       
      # test the the ANOVA function that I will write works
      #  all.pvals <- NULL
      #   for (iSite in 1:(ncol(train.data) - 1)){
      #     curr.data <- train.data[, iSite]
      #     all.pvals[iSite] <- anova(lm(curr.data ~ train.data$labels))$Pr[1]
      #  }

      # an alternative way - still is as slow
      # get.anova.pvals <- function(iSite) {
      #   anova(lm(train.data[, iSite] ~ train.data$labels))$Pr[1]
      # }
      # all.pvals <- sapply(grep("site", names(train.data)), get.anova.pvals)
       
       

     # writing the ANOVA function myself to speed it up (as I did in MATLAB)

     # get the ANOVA p-values for all sites...   
     num.points.in.each.group <- train.data %>% group_by(labels) %>% summarize(n = n())
     num.sites <- dim(train.data)[2] - 1
     num.groups <- dim(num.points.in.each.group)[1]    # the number of classes
     
     group.means <- aggregate(train.data[, 1:num.sites], list(train.data$labels), mean)  # slowest part of the code...

     
     MSS <- apply(sweep(scale(select(group.means, starts_with("site")), scale = FALSE)^2, 1, num.points.in.each.group$n, FUN = "*"), 2, sum)
     TSS <- apply(scale(select(train.data, -labels), scale = FALSE)^2, 2, sum)
     SSE <- TSS - MSS   # residual SS = total SS + model SS
     
     between.deg.free <- num.groups - 1
     within.deg.free <- dim(train.data)[1] - num.groups
     
     f.stats <- (MSS/between.deg.free)/(SSE/within.deg.free)
     all.pvals <- pf(f.stats, df1 = between.deg.free, df2 = within.deg.free, lower.tail = FALSE)
     
     

     # find the sites with the k smallest p-values
     sorted.data <- sort(all.pvals, index.return = TRUE)
       
     sites.to.use <- sorted.data$ix
      

     # if excluding selective sites, first remove these num.sites.to.exclude sites 
     # (before using only the self$num.sites.to.use)
      if (!is.na(self$num.sites.to.exclude)) {
        sites.to.use <- sites.to.use[(self$num.sites.to.exclude  + 1):num.sites]
      }
      
      # use only the num.sites.to.use most selective sites
      if (!is.na(self$num.site.to.use)) {
        sites.to.use <- sites.to.use[1:self$num.site.to.use]
      }
      
       
       # return a list with the results
       processed.data <- list(train.data = cbind(train.data[sites.to.use], labels = train.data$labels), 
                              test.data = cbind(test.data[sites.to.use], labels = test.data$labels, time = test.data$time))
          
       
       return(processed.data)
       
          
     }  # end preprocess_data


     
  ) # end public properties/methods
  
  
)  # end class









