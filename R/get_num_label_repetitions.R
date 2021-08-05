#' @title Get the number of trial repetitions for a given label
#'
#' @description
#' Calculates how many repeated trials there are for each label level. This can
#' be useful for selecting sites that have a minimum number of repetitions of
#' each stimulus or other experimental condition.
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param variable_to_use A string specifying whether label variable should be
#'   used for calculating the minimum number of level repetitions.
#'
#' @param levels_to_use A character vector specifying which levels to include.
#' If not set, all levels will be used.
#'
#' @note The returned value is an S3 object that inherits from data.frame that
#' has an associated plot() method.
#'
#' @examples
#' data_file <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NeuroDecodeR")
#' label_rep_info <- get_num_label_repetitions(data_file, "stimulus_ID")
#' plot(label_rep_info)
#'
#'
#' @export
get_num_label_repetitions <- function(binned_data,
                                      variable_to_use,
                                      levels_to_use = NULL) {


  # load data if a string is given and check that the data is in binned format
  binned_data <- check_and_load_binned_data(binned_data)

  
  # get the site_info which will be returned along with num label repetitions 
  #  to allow additional filtering
  site_info <- binned_data %>%
    select("siteID", starts_with("site_info")) 

  
  # select only the siteID and labels we want to count number of repetitions
  binned_data <- select(binned_data, .data$siteID, label = paste0("labels.", variable_to_use))


  if (is.null(levels_to_use)) {
    levels_to_use <- levels(binned_data$label)
  }

  # test levels_to_use specified are all levels that exist
  invalid_levels_specified <- setdiff(levels_to_use, levels(binned_data$label))
  if (length(invalid_levels_specified) != 0) {
    stop(paste(
      "The level(s)", paste0("'", invalid_levels_specified, "'"),
      "do not exist in the variable", variable_to_use))
  }


  # reduce data to only the levels specified
  binned_data <- dplyr::filter(binned_data, binned_data$label %in% levels_to_use)

  num_repeats_per_level <- binned_data %>%
    dplyr::group_by(.data$siteID, .data$label) %>%
    count() %>%
    dplyr::mutate(label = paste0("level.", .data$label))
  

  min_num_repeats_per_level <- num_repeats_per_level %>%
    dplyr::group_by(.data$siteID) %>%
    dplyr::summarize(min_repeats = min(n)) 

  
  label_rep_obj <- tidyr::pivot_wider(num_repeats_per_level, 
                                       names_from = .data$label, 
                                       values_from = n) %>%
    dplyr::full_join(min_num_repeats_per_level, by = "siteID") %>%
    dplyr::select(.data$siteID, .data$min_repeats, everything())  
  
  
  
  
  # add the site_info to the label_rep_obj
  
  
  max_unique_site_info_vals <- site_info %>%
    dplyr::group_by(.data$siteID) %>%
    dplyr::summarize(across(everything(), n_distinct)) %>%
    dplyr::summarize(across(everything(), max)) %>%
    dplyr::select(-.data$siteID) %>%
    tidyr::pivot_longer(everything()) 
  
  
  # include site_info variables that only have a single value for each site
  site_info_variables_names_to_include <- max_unique_site_info_vals %>%
    dplyr::filter(.data$value == 1) %>%
    dplyr::pull(.data$name)
  
  
  # if for a given site_info. variable, if the value is not unique for each site on all trials
  #  send a message that these site_info variables are not available to filter a site on
  if (length(site_info_variables_names_to_include) != nrow(max_unique_site_info_vals)) {
    
    site_info_variables_names_not_to_include <- max_unique_site_info_vals %>%
      dplyr::filter(.data$value != 1) %>%
      dplyr::pull(.data$name) 
    
    # print out that particular site_info variables different values in
    # different rows a single for a given site so can't uniquely select a site
    # based on the value of a given variable
    paste("The following site_info variables have more than one value in a row for a specific site", 
                  "so one cannot select sites based on the value of these variables:\n", 
                  paste(site_info_variables_names_not_to_include, collapse = ", "))
  }   
  
  
  
  # add the site info variables to the label_rep_info so that one can filter particular sites
  #  based on site_info values
  site_info_to_use <- site_info %>%
    dplyr::select("siteID", all_of(site_info_variables_names_to_include)) %>%
    dplyr::group_by(.data$siteID) %>%
    dplyr::summarize(across(everything(), dplyr::first))
  
  
  label_rep_obj <- left_join(label_rep_obj, site_info_to_use, "siteID")
  

  # making this an S3 object so that it can have an associated plot() function
  attr(label_rep_obj, "class") <- c("label_repetition", "data.frame")
  attr(label_rep_obj, "variable_used") <- variable_to_use


  label_rep_obj

}




#' A plot function for label_repetition object
#'
#' This function plots the number of sites have at least k label repetitions.
#' Creating this plot is useful for assessing how to set the number of
#' cross-validation splits (and repeats of labels per cross-validation split) to
#' use in a datasource. This function returns a ggplot2 object which can be
#' further modified as needed.
#'
#'
#' @param x A label_repetition object that was generated from calling the
#'   get_num_label_repetitions() function.
#' 
#'
#' @param ... This is needed to conform to the plot generic interface.
#'
#' @param show_legend A Boolean specifying whether to show a legend 
#' that list which label each color in the plot corresponds to. 
#' 
#' @param site_info_facet_name Setting this to a string that is the name of one of the site_info
#'  variable names will make a plot that is faceted by this variable name; i.e., it will
#'  show the number of number of sites have at least k label repetitions separately for each level
#'  in the site_info variable name specified.
#' 
#' 
#' @export
plot.label_repetition <- function(x, ..., show_legend = TRUE, site_info_facet_name = NULL) {

  
  # add string "site_info." to the front of site_info_facet_name if it doesn't
  # already start with "site_info."
  if (!is.null(site_info_facet_name)) {
    if (!grepl("^site_info.", site_info_facet_name)) {
      site_info_facet_name <- paste0("site_info.", site_info_facet_name)
    }
  }

  
  # extract label name that we have info about the number of repetitions
  variable_used <- attr(x, "variable_used")


  num_sites_with_k_repeats <- get_num_sites_with_k_label_repetitions(x, site_info_facet_name)


  # pivot_longer() is giving an error message here so sticking with gather()
  #num_sites_with_k_repeats_long <- num_sites_with_k_repeats %>%
  #  tidyr::pivot_longer(-starts_with("grouping_var", "label")) %>%
  #  mutate(min_reps = as.numeric(.data$min_reps))

  num_sites_with_k_repeats_long <- num_sites_with_k_repeats %>%
    tidyr::gather("min_reps", "num_sites", -.data$label, -.data$grouping_var) %>%
    mutate(min_reps = as.numeric(.data$min_reps))  
  
  
  # set breaks at integer increments 
  # if more than 20 labels, x-axis lines at every other value, etc.
  break_increments <- max(round(max(x$min_repeats)/20), 1)
  break_lines <- seq(0, max(x$min_repeats), by = break_increments)
  
  
  # just plot the minimum number of repeats
  g <- num_sites_with_k_repeats_long %>%
    dplyr::filter(.data$label != "min_repeats") %>%
    dplyr::mutate(label = gsub("level.", "", .data$label)) %>%
    ggplot(aes(.data$min_reps, .data$num_sites, col = .data$label)) +
    geom_line() +
    geom_point() +
    xlab("Number of repeated conditions") +
    ylab("Number of sites") +
    ggtitle(paste("Label:", variable_used)) + 
    scale_x_continuous(breaks= break_lines) + 
    theme(panel.grid.minor.x = element_blank())   # put break lines at integer values

  
  # argument to turn off the legends
  # could make this the default if there are too many labels...
  if (show_legend == FALSE) {
    g <- g + 
      theme(legend.position = "none")
  }
  
  
  
  # add the facet and change plot title
  if (!is.null(site_info_facet_name)) {
    g <- g + 
      facet_wrap(~grouping_var) + 
      ggtitle(paste("Label:", variable_used, "     Grouped by:", 
                    gsub("site_info.", "", site_info_facet_name)))  
  }
  
  
  g

  
}




# a helper function that returns how many sites have k repeats for all labels
# this is used above to plot the data
get_num_sites_with_k_label_repetitions <- function(label_rep_obj, site_info_grouping_name) {


  if (is.null(site_info_grouping_name)) {
    
    label_rep_obj <- dplyr::mutate(label_rep_obj, grouping_var = 1) %>%
      dplyr::select(-starts_with("site_info"))
    
  } else {
    
    label_rep_obj <- dplyr::select(label_rep_obj, 
                                   -starts_with("site_info"), 
                                   grouping_var = {{site_info_grouping_name}})
  }
  

  
  # could perhaps just make the grouping_var a constant if no grouping variable was given
  #  the remove this at the end...
  
  
  max_min_num_reps <- max(label_rep_obj$min_repeats)

  
  label_rep_long <- tidyr::pivot_longer(label_rep_obj, 
                                        starts_with("level"), 
                                        names_to = "label", 
                                        values_to = "num_reps")
  
  
  num_sites_with_k_repeats <- label_rep_long %>%
    dplyr::group_by(.data$grouping_var, .data$label) %>%
    dplyr::summarize("0" = sum(.data$num_reps >= 0))
  
  
  for (k in 1:max_min_num_reps) {

    curr_count <- label_rep_long %>%
      dplyr::group_by(.data$grouping_var, .data$label, .drop = TRUE) %>%
      dplyr::summarize(num_with_k_reps = sum(.data$num_reps >= k)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$num_with_k_reps)

    names(curr_count) <- as.character(k)      # paste0("reps_", sprintf('%03d', k))

    num_sites_with_k_repeats <- cbind(num_sites_with_k_repeats, curr_count)
    
  }

  num_sites_with_k_repeats

}
