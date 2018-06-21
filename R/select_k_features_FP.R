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
#' \item{\code{select_k_features_FP$new(num_site_to_use, num_sites_to_exclude)}}{
#' This constructor uses num_site_to_use of the best sites as found via an ANOVA. 
#' Additionally, it can eliminate the best num_sites_to_exclude to use sites again
#' using an ANOVA. If both num_site_to_use and num_sites_to_exclude are set, then 
#' num_sites_to_exclude will first be eliminated and then the next num_site_to_use will 
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
     num_site_to_use = NA,
     num_sites_to_exclude = NA,
     
     # constructor
     initialize = function(num_site_to_use, num_sites_to_exclude) {
       
       if (!missing(num_site_to_use)) {
         self$num_site_to_use <- num_site_to_use
       }
       
       if (!missing(num_sites_to_exclude)) {
         self$num_sites_to_exclude <- num_sites_to_exclude
       }
       
     },
                      
     # methods
     preprocess_data = function(train_data, test_data){
       if (is.na(self$num_site_to_use) && is.na(self$num_sites_to_exclude)) {
         stop('Either num_site_to_use or num_sites_to_exclude must be set prior to calling the preprocess_data method')
       }

      # test the the ANOVA function that I will write works
      #  all_pvals <- NULL
      #   for (iSite in 1:(ncol(train_data) - 1)){
      #     curr_data <- train_data[, iSite]
      #     all_pvals[iSite] <- anova(lm(curr_data ~ train_data$labels))$Pr[1]
      #  }

      # an alternative way - still is as slow
      # get_anova_pvals <- function(iSite) {
      #   anova(lm(train_data[, iSite] ~ train_data$labels))$Pr[1]
      # }
      # all_pvals <- sapply(grep("site", names(train_data)), get_anova_pvals)
       
     # writing the ANOVA function myself to speed it up (as I did in MATLAB)

     # get the ANOVA p-values for all sites...   
     num_points_in_each_group <- train_data %>% group_by(labels) %>% summarize(n = n())
     num_sites <- dim(train_data)[2] - 1
     num_groups <- dim(num_points_in_each_group)[1]    # the number of classes
     
     # group_means <- select(aggregate(train_data[, 1:num_sites], list(train_data$labels), mean), starts_with("site))  # slowest part of the code...
     # another option that is just as fast...
     # group_means <- train_data %>% group_by(labels) %>% summarise_each(funs(mean)) %>% select(starts_with("site"))
     
     # marginally faster way to compute the group means (might have more of a speed up if more sites are used)
     split_data <- split(train_data[, 1:num_sites], train_data$labels)
     group_means <- t(sapply(split_data, function(one_group_data) apply(one_group_data, 2, mean)))
     MSS <- apply(sweep(scale(group_means, scale = FALSE)^2, 1, num_points_in_each_group$n, FUN = "*"), 2, sum)
     TSS <- apply(scale(select(train_data, -labels), scale = FALSE)^2, 2, sum)
     SSE <- TSS - MSS   # residual SS = total SS + model SS
     between_deg_free <- num_groups - 1
     within_deg_free <- dim(train_data)[1] - num_groups
     f_stats <- (MSS/between_deg_free)/(SSE/within_deg_free)
     all_pvals <- pf(f_stats, df1 = between_deg_free, df2 = within_deg_free, lower.tail = FALSE)
     # find the sites with the k smallest p-values
     sorted_data <- sort(all_pvals, index.return = TRUE)
     sites_to_use <- sorted_data$ix
     
     # if excluding selective sites, first remove these num_sites_to_exclude sites 
     # (before using only the self$num_sites_to_exclude)
      if (!is.na(self$num_sites_to_exclude)) {
        sites_to_use <- sites_to_use[(self$num_sites_to_exclude  + 1):num_sites]
      }
      
      # use only the num_site_to_use most selective sites
      if (!is.na(self$num_site_to_use)) {
        sites_to_use <- sites_to_use[1:self$num_site_to_use]
      }
     
       # return a list with the results
       processed_data <- list(train_data = cbind(train_data[sites_to_use], labels = train_data$labels), 
                              test_data = cbind(test_data[sites_to_use], labels = test_data$labels, time = test_data$time))
          
       return(processed_data)
     }  # end preprocess_data
  ) # end public properties/methods
)  # end class