#' A feature preprocessor (FP) that reduces the data to the best k features
#'
#' This feature preprocessor object applies an ANOVA to the training data to
#' find the p-value of all features. It then either uses the top k features with
#' the smallest p-values, or it removes the features with the smallest k
#' p-values. Additionally, this function can be used to remove the top k
#' p-values and then use only the following j next smallest p-values (for
#' example, this can be useful if one is interesting in comparing the
#' performance using the most selective 10 neurons to using the next 10 most
#' selective neurons, etc.).
#'
#' @param num_site_to_use The number of features with the smallest p-values to use. 
#'
#' @param num_sites_to_exclude The number of features with the smallest p-values
#'   that should be excluded.
#' 
#' @examples
#' # This will cause the cross-validator use only the 50 most selective sites
#' fp <- fp_select_k_features(num_site_to_use = 50)
#' 
#' # This will cause the cross-validator to remove the 20 most selective sites
#' fp <- fp_select_k_features(num_sites_to_exclude = 20)
#'
#' # This will cause the cross-validator to remove the 20 most selective sites 
#' # and then use only the 50 most selective sites that remain after the 20 are 
#' # eliminated 
#' fp <- fp_select_k_features(50, 20)
#' 
#'
#' @family feature_preprocessor



# the constructor 
#' @export
fp_select_k_features <- function(num_site_to_use = NA,
                                 num_sites_to_exclude = NA){
  
  if (is.na(num_site_to_use) && is.na(num_sites_to_exclude)) {
    stop(paste0('Either num_site_to_use or num_sites_to_exclude must be set',
                'prior to calling the preprocess_data method'))
  }
  
  
  the_fp <- list(num_site_to_use = num_site_to_use,
                 num_sites_to_exclude = num_sites_to_exclude)
  
  attr(the_fp, "class") <- "fp_select_k_features"
  the_fp
}




#' @export
preprocess_data.fp_select_k_features = function(fp, training_set, test_set){


# writing the ANOVA function myself so that it runs faster

# get the ANOVA p-values for all sites...   
num_points_in_each_group <- training_set %>% group_by(train_labels) %>% summarize(n = n())
num_sites <- dim(training_set)[2] - 1
num_groups <- dim(num_points_in_each_group)[1]    # the number of classes

# marginally faster way to compute the group means (might have more of a speed up if more sites are used)
split_data <- split(training_set[, 1:num_sites], training_set$train_labels)
group_means <- t(sapply(split_data, function(one_group_data) apply(one_group_data, 2, mean)))

MSS <- apply(sweep(scale(group_means, scale = FALSE)^2, 1, num_points_in_each_group$n, FUN = "*"), 2, sum)
TSS <- apply(scale(select(training_set, -train_labels), scale = FALSE)^2, 2, sum)
SSE <- TSS - MSS   # residual SS = total SS + model SS

between_deg_free <- num_groups - 1
within_deg_free <- dim(training_set)[1] - num_groups

f_stats <- (MSS/between_deg_free)/(SSE/within_deg_free)
all_pvals <- pf(f_stats, df1 = between_deg_free, df2 = within_deg_free, lower.tail = FALSE)

# find the sites with the k smallest p-values
sorted_data <- sort(all_pvals, index.return = TRUE)
sites_to_use <- sorted_data$ix


# if excluding selective sites, first remove these num_sites_to_exclude sites 
if (!is.na(fp$num_sites_to_exclude)) {
  sites_to_use <- sites_to_use[(fp$num_sites_to_exclude  + 1):num_sites]
}

# use only the num_site_to_use most selective sites
if (!is.na(fp$num_site_to_use)) {
  sites_to_use <- sites_to_use[1:fp$num_site_to_use]
}



# returning information about which sites were selected
#  this could be saved by the cross-validator to find out which sites were used
selected_site <- rep(FALSE, dim(select(training_set, starts_with("site")))[2])
selected_site[sites_to_use] <- TRUE
siteID <- as.numeric(substr(names(select(training_set, starts_with("site"))), 6, 100))
fp_info <- data.frame(siteID = siteID,
                            pvals = all_pvals, 
                            selected_site = selected_site)


 # return a list with the preprocessed data
 processed_data <- list(training_set = cbind(training_set[sites_to_use], 
                                             train_labels = training_set$train_labels), 
                        test_set = cbind(test_set[sites_to_use], test_labels = test_set$test_labels, 
                                         time_bin = test_set$time_bin), 
                        fp_info = fp_info)
    
 processed_data
 
}  



#' @export
get_parameters.fp_select_k_features = function(fp){
    data.frame(fp_select_k_features.num_site_to_use = fp$num_site_to_use,
             fp_select_k_features.num_sites_to_exclude = fp$num_sites_to_exclude)
}





