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
#' \item{\code{basic_DS$new(binned_data, specific_binned_label_name, num_cv_splits, use_count_data, num_times_to_repeat_labels_per_cv_block )}}{
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
    binned_data = NA,
    specific_binned_label_name = NA, 
    num_cv_splits = NA,
    use_count_data = FALSE,
    num_times_to_repeat_labels_per_cv_block = 1,
    
    # constructor
    initialize = function(binned_file_name, specific_binned_label_name, num_cv_splits, use_count_data = FALSE) {
      self$specific_binned_label_name <- specific_binned_label_name
      self$num_cv_splits <- num_cv_splits
      
      # load the binned data  
      load(binned_file_name)
      
      # now called binned_data binned_data, should really refactor my code to my this change throughout 
      if (!exists("binned_data")) {
        binned_data <- binned.data
      }
      if (use_count_data) {
        binned_data <- convert_rates_to_counts(binned_data) 
      }
      self$binned_data <- binned_data
    },
    
    # methods
    get_data = function(){
      # defining these here to make it potentially easy to transfer my code to other R OO systems 
      # (at the cost of a little memory)
      binned_data <- self$binned_data 
      specific_binned_label_name <- self$specific_binned_label_name
      num_trials_used_per_label <- self$num_cv_splits * self$num_times_to_repeat_labels_per_cv_block 

      # remove all labels that aren't being used, and rename the labels that are being used "labels"
      label_col_ind <- match(paste0("labels_", specific_binned_label_name), names(binned_data))
      binned_data <- binned_data %>% select(siteID, starts_with("time"), labels = label_col_ind)  
      
      # order data by: repetitions, sites, labels
      all_k_fold_data <- binned_data  %>% group_by(labels, siteID) %>% sample_n(size = num_trials_used_per_label)
      unique_labels <- unique(all_k_fold_data$labels)
      num_sites <- length(unique(binned_data$siteID))  
      num_time_bins <- sum(grepl("time_*", names(binned_data)))
      num_labels <- length(unique_labels)
      
      # add a few names in the data frame
      
      # CV_slice_ID is a groups of data that have one example for each label
      #  - these groups are mapped into CV blocks where blocks contain num_times_to_repeat_labels_per_cv_block of each label  
      CV_slice_ID <- rep(1:num_trials_used_per_label, num_labels * num_sites)
      
      # add the number of the cross-validitation split to the data ...
      all_k_fold_data$CV_slice_ID <- CV_slice_ID
      
      # paste the site.000 in front of the siteID so that is is listed as site_0001, site_0002, etc
      all_k_fold_data$siteID <- paste0("site_", stringr::str_pad(all_k_fold_data$siteID, 4, pad = "0"))

      # reshape the data so that it's [label*time*cv x site]  data frame 
      # can do this quickly using the reshape2 package!
      
      melted_data <- reshape2::melt(all_k_fold_data, id.vars = c("siteID", "labels", "CV_slice_ID"), 
                          variable.name = "time", value.name = "activity")
      
      all_cv_data <- reshape2::dcast(melted_data, labels + time + CV_slice_ID ~ siteID, value.var = "activity")
      
      # below I changed this so can put repeats of labels in a CV split...
      # # create different CV_1, CV_2 which list which points are training points and which points are test points
      # for (iCV in 1:num_cv_splits) {
      #   eval(parse(text=paste0("all_cv_data$CV_", iCV, "= ifelse(all_cv_data$CV_slice_ID == iCV, 'test', 'train')")))
      # }
      # all_cv_data <- select(all_cv_data, -CV_slice_ID)  # remove the original CV_slice_ID field     

      # create different CV_1, CV_2 which list which points are training points and which points are test points
      for (iCV in 1:self$num_cv_splits) {
        start_ind <- (((iCV - 1) * self$num_times_to_repeat_labels_per_cv_block) + 1)
        end_ind <- (iCV * self$num_times_to_repeat_labels_per_cv_block)
        curr_cv_block_inds <- start_ind:end_ind
        eval(parse(text=paste0("all_cv_data$CV_", iCV, "= ifelse(all_cv_data$CV_slice_ID %in% curr_cv_block_inds, 'test', 'train')")))
      }
      
      all_cv_data <- select(all_cv_data, -CV_slice_ID)  # remove the original CV_slice_ID field     
      
      return(all_cv_data)
    }  # end get_data() 
  )  # end public data/methods
)  # end for the class




