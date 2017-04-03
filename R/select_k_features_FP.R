#' A feature preprocessor (FP) that reduces the data to the best k features
#'
#' This feature prerpocessor object find the k most selective features using an ANOVA on the training data. 
#' The proprocessor then eleminates all other features in both the training and test sets. This preprocessor
#' can also eliminate the best k features. 
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section select_k_features_FP:
#' 
#' \describe{
#' \item{\code{select_k_features_FP$new(num.site.to.use, num.sites.to.exclude)}}{
#' This constructor uses num.site.to.use of the best sites as found via an ANOVA. 
#' Additionally, it can eliminate the best num.sites.to.exclude to use sites again
#' using an ANOVA. If both num.site.to.use and num.sites.to.exclude are set, then 
#' num.sites.to.exclude will first be eliminated and then the next num.site.to.use will 
#' be kept. If successful, will return a new \code{select_k_features_FP} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{preprocess_data}}{
#' Like all FP objects, this method finds parameters on the training set and then applies them 
#' to the training and test sets. For select_k_features_FP, the parameters found on the training set are 
#' the sites that are the most selective, and these sites are then kept and/or eliminated on training and 
#' test sets.
#' }}
#' 
#' 
#' 
#' @import R6
#' @export





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
     
     # group.means <- select(aggregate(train.data[, 1:num.sites], list(train.data$labels), mean), starts_with("site))  # slowest part of the code...
     # another option that is just as fast...
     # group.means <- train.data %>% group_by(labels) %>% summarise_each(funs(mean)) %>% select(starts_with("site"))
     
     
     # marginally faster way to compute the group means (might have more of a speed up if more sites are used)
     split.data <- split(train.data[, 1:num.sites], train.data$labels)
     group.means <- t(sapply(split.data, function(one.group.data) apply(one.group.data, 2, mean)))

     
     MSS <- apply(sweep(scale(group.means, scale = FALSE)^2, 1, num.points.in.each.group$n, FUN = "*"), 2, sum)
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









