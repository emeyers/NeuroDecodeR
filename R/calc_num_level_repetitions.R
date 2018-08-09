#' @title calc_num_level_repetitions
#'s
#' @description  This function takes in labels in binned label format, and an integer k, 
#' and returns the indices for all sites (e.g. neurons) that have at least
#' k presentations of each condition.  
#'
#' @param binned_data Should be a dataframe in binned format.
#' @param variable_to_use A string that indicates what variable to analyze.
#' @param levels_to_use A single string or a vector of strings.
#' @param k_value  Returns indices for all sites (e.g. neurons) that have at least
#'   k presentations of each condition.  
#'
#' @example s
#' 
#' @return 
#' \item{num_repeats_each_level}{This parameter list for all sites the number of 
#'      repetitions present for each.}
#' \item{matrix_num_repeats_each_level}{A [site-ID x level_repeats] matrix that 
#'      specifies for each the number of repetitions of each level for each site 
#'      (this variable could be useful for determining if particular conditions 
#'      should be excluded based on whether they were presented only a few times to many sites)}    
#' \item{min_num_repeats_all_sites}{This parameter lists the minimum number of
#'      repetitions for each site.}
#'
#' 
#' @import R6
#' @export



calc_num_level_repetitions <- function(binned_data, variable_to_use, levels_to_use = NULL, k = 0) { 
  
  
  # dealing with dplyr nonstandard evaluation... 
  binned_data <- select(binned_data, siteID, labels = variable_to_use)
 

  if (is.null(levels_to_use)) {
    levels_to_use <- levels(binned_data$labels)
  } else {
    # Test if levels_to_use given is valid
    contain_all = TRUE
    for (level in levels_to_use) {
      if(!(level %in% levels(binned_data$labels))){
        contain_all = FALSE
      }
    }
    if (!contain_all) {
      stop("Error: invalid level_to_use")
    }
  }

  
  
  binned_data <- filter(binned_data, binned_data$labels %in% levels_to_use)
  
  
  num_repeats_each_level <- binned_data %>% 
   group_by(siteID, labels) %>%
    count()
  
  
  matrix_num_repeat_each_level <- spread(num_repeats_each_level, labels, n)
  
  min_num_repeates_each_site <- num_repeats_each_level %>%
    group_by(siteID) %>%
     summarize(min_repeats = min(n))
  
  sites_with_at_least_k_repeats <- filter(min_num_repeates_each_site, min_repeats >= k)

  level_repetition_info <- NULL
  level_repetition_info$num_repeats_each_level <- num_repeats_each_level
  level_repetition_info$matrix_num_repeat_each_level <- matrix_num_repeat_each_level
  level_repetition_info$min_num_repeates_each_site <- min_num_repeates_each_site
  level_repetition_info$sites_with_at_least_k_repeats <- sites_with_at_least_k_repeats
  level_repetition_info$levels_used <- levels_to_use
  
  return(level_repetition_info)
  
} #end calc_num_level_repetitions




# things to do: 
# 1: test that the argument levels_to_use works 
#  -> add error message if levels specified to not exist in the variable









