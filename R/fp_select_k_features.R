#' A feature preprocessor (FP) that reduces data to the k most selective features
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
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the fp_select_k_features feature preprocessor work with the
#'   pipe (|>) operator. This argument should almost never be directly
#'   set by the user to anything other than NULL. If this is set to the default
#'   value of NULL, then the constructor will return a fp_select_k_features
#'   object. If this is set to an ndr container, then a fp_select_k_features
#'   object will be added to the container and the container will be returned.
#'   If this argument is set to another ndr object, then both that ndr object as
#'   well as a new fp_select_k_features object will be added to a new container
#'   and the container will be returned.
#'
#' @param num_sites_to_use The number of features with the smallest p-values to use.
#'
#' @param num_sites_to_exclude The number of features with the smallest p-values
#'   that should be excluded.
#'   
#' @return This constructor creates an NDR feature preprocessor object with the
#'   class `fp_select_k_features`. Like all NDR feature preprocessor objects,
#'   this feature preprocessor will be used by the cross-validator to
#'   pre-process the training and test data sets.
#'   
#'
#' @examples
#' # This will cause the cross-validator use only the 50 most selective sites
#' fp <- fp_select_k_features(num_sites_to_use = 50)
#'
#' # This will cause the cross-validator to remove the 20 most selective sites
#' fp <- fp_select_k_features(num_sites_to_exclude = 20)
#'
#' # This will cause the cross-validator to remove the 20 most selective sites
#' # and then use only the 50 most selective sites that remain after the 20 are
#' # eliminated
#' fp <- fp_select_k_features(num_sites_to_use = 50, num_sites_to_exclude = 20)
#' @family feature_preprocessor
#'
#'
# the constructor
#' @export
fp_select_k_features <- function(ndr_container_or_object = NULL, 
                                 num_sites_to_use = NA,
                                 num_sites_to_exclude = NA) {
  if (is.na(num_sites_to_use) && is.na(num_sites_to_exclude)) {
    stop(paste0(
      "Either num_sites_to_use or num_sites_to_exclude must be set",
      "prior to calling the preprocess_data method"
    ))
  }


  the_fp <- list(
    num_sites_to_use = num_sites_to_use,
    num_sites_to_exclude = num_sites_to_exclude
  )

  attr(the_fp, "class") <- "fp_select_k_features"
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the feature preprocessor in it
  put_ndr_object_in_container(ndr_container_or_object, the_fp)
  
  
}





#' @inherit preprocess_data
#' @keywords internal
#' @export
preprocess_data.fp_select_k_features <- function(fp, training_set, test_set) {


  # writing the ANOVA function myself so that it runs faster

  # get the ANOVA p-values for all sites...
  num_points_in_each_group <- training_set %>%
    group_by(.data$train_labels) %>%
    summarize(n = n())
  num_sites <- dim(training_set)[2] - 1
  num_groups <- dim(num_points_in_each_group)[1] # the number of classes

  # marginally faster way to compute the group means
  split_data <- split(training_set[, 1:num_sites], training_set$train_labels)
  group_means <- t(sapply(split_data, function(one_group_data) apply(one_group_data, 2, mean)))

  MSS <- apply(sweep(scale(group_means, scale = FALSE)^2, 1,
                     num_points_in_each_group$n, FUN = "*"), 2, sum)
  TSS <- apply(scale(select(training_set, -.data$train_labels), scale = FALSE)^2, 2, sum)
  SSE <- TSS - MSS # residual SS = total SS + model SS

  between_deg_free <- num_groups - 1
  within_deg_free <- dim(training_set)[1] - num_groups

  f_stats <- (MSS / between_deg_free) / (SSE / within_deg_free)
  all_pvals <- pf(f_stats, df1 = between_deg_free, df2 = within_deg_free, 
                  lower.tail = FALSE)

  # find the sites with the k smallest p-values
  sorted_data <- sort(all_pvals, index.return = TRUE)
  sites_to_use <- sorted_data$ix


  # if excluding selective sites, first remove these num_sites_to_exclude sites
  if (!is.na(fp$num_sites_to_exclude)) {
    sites_to_use <- sites_to_use[(fp$num_sites_to_exclude + 1):num_sites]
  }

  # use only the num_sites_to_use most selective sites
  if (!is.na(fp$num_sites_to_use)) {
    sites_to_use <- sites_to_use[1:fp$num_sites_to_use]
  }



  # Returning information about which sites were selected. This could 
  #  be saved by the cross-validator to find out which sites were used.
  selected_site <- rep(FALSE, dim(select(training_set, starts_with("site")))[2])
  selected_site[sites_to_use] <- TRUE
  siteID <- as.numeric(substr(names(select(training_set, starts_with("site"))), 6, 100))
  fp_info <- data.frame(
    siteID = siteID,
    pvals = all_pvals,
    selected_site = selected_site
  )


  # return a list with the preprocessed data
  processed_data <- list(
    training_set = cbind(training_set[sites_to_use],
      train_labels = training_set$train_labels
    ),
    test_set = cbind(test_set[sites_to_use],
      test_labels = test_set$test_labels,
      time_bin = test_set$time_bin
    ),
    fp_info = fp_info
  )

  processed_data
}





#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.fp_select_k_features <- function(ndr_obj) {
  data.frame(
    fp_select_k_features.num_sites_to_use = ndr_obj$num_sites_to_use,
    fp_select_k_features.num_sites_to_exclude = ndr_obj$num_sites_to_exclude
  )
}
