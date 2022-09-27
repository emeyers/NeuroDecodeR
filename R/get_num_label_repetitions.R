

#' @title Get the number of sites have at least k trials of each label level
#' 
#' @description Calculates number of sites that have at least k label level
#' repetitions for all values k. This information is useful for assessing how to
#' set the number of cross-validation splits (and repeats of labels per
#' cross-validation split) to use in a datasource. One can also assess the
#' number of label level repetitions separately conditioned on another site_info
#' variable. For example, if one has recordings from different brain regions,
#' and the brain region information is contained in a site_info variable, then
#' one could calculate how many sites have at least k repetitions for each
#' stimulus in each brain region.
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param labels A string specifying which label variable should be
#'   used for calculating the minimum number of level repetitions.
#'   
#' @param site_info_grouping_name A character string that specifies if the
#'   number of sites that have k repetitions should be computed separately 
#'   based on the levels of a site_info variable.
#'   
#' @param label_levels A character vector specifying which levels to include.
#'   If not set, all levels will be used. 
#'   
#' @return  A data frame with the class `label_repetition` which allows the
#'   results to be plotted. The returned data frame has a row for each label
#'   level, and columns with sequential integer values k = 0, 1, ... The values
#'   in the data frame show the number of sites that have at least k repetitions
#'   of a given stimulus.
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
                                      labels,
                                      site_info_grouping_name = NULL,
                                      label_levels = NULL) {
  
  
  label_rep_each_site_df <- get_num_label_repetitions_each_site(binned_data, labels, label_levels)
  
  
  
  
  # calculate how many level repetitions there are
  
  
  if (is.null(site_info_grouping_name)) {
    
    label_rep_each_site_df <- dplyr::mutate(label_rep_each_site_df, grouping_var = 1) %>%
      dplyr::select(-starts_with("site_info"))
    
  } else {
    
    
    # If the site_info_grouping_name does not start with the term "site_info." append it.
    # This will allow the user to skip writing out site_info. and just give the name after it
    if (!grepl("^site_info.", site_info_grouping_name)) {
      site_info_grouping_name <- paste0("site_info.", site_info_grouping_name)
    }
    

    
    # check that the grouping variable name is in the site_info variables
    # if not throw an error
    site_info_names <- names(dplyr::select(dplyr::ungroup(label_rep_each_site_df), starts_with("site_info")))
    if (!(site_info_grouping_name %in% site_info_names)) {
      
      stop(paste("The argument site_info_grouping_name must be one of the site_info names,",
                 "i.e., it must be set to one of the following: \n", 
                 paste(site_info_names, collapse = ", ")))
    }
    
    
    label_rep_each_site_df <- label_rep_each_site_df %>% 
      dplyr::select(-starts_with("site_info"), grouping_var = {{site_info_grouping_name}})
    
  }
  
  
  # could perhaps just make the grouping_var a constant if no grouping variable was given
  #  the remove this at the end...
  
  
  max_min_num_reps <- max(label_rep_each_site_df$min_repeats)
  
  
  label_rep_long <- tidyr::pivot_longer(label_rep_each_site_df, 
                                        starts_with("level"), 
                                        names_to = "label", 
                                        values_to = "num_reps")
  
  
  num_label_repeats_obj <- label_rep_long %>%
    dplyr::group_by(.data$grouping_var, .data$label) %>%
    dplyr::summarize("0" = sum(.data$num_reps >= 0))
  
  
  
  for (k in 1:max_min_num_reps) {
    
    curr_count <- label_rep_long %>%
      dplyr::group_by(.data$grouping_var, .data$label, .drop = TRUE) %>%
      dplyr::summarize(num_with_k_reps = sum(.data$num_reps >= k)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$num_with_k_reps)
    
    names(curr_count) <- as.character(k)      # paste0("reps_", sprintf('%03d', k))
    
    num_label_repeats_obj <- cbind(num_label_repeats_obj, curr_count)
    
  }
  
  
  
  # add info on number of sites that have at least k repetitions for all levels
  
  get_num_sites_with_k_reps_all_levels <- function(k) {
    
    curr_all_levels_num_sites_with_at_least_k_reps <- label_rep_each_site_df %>%
      dplyr::group_by(.data$grouping_var) %>%   
      dplyr::mutate(max_sites = n()) %>% 
      dplyr::summarize(k = sum(.data$min_repeats >= k)) 
    
    names(curr_all_levels_num_sites_with_at_least_k_reps) <- c(names(curr_all_levels_num_sites_with_at_least_k_reps[1]), as.character(k))  
    
    curr_all_levels_num_sites_with_at_least_k_reps
    
  }
  
  
  num_sites_with_k_reps_all_levels <- get_num_sites_with_k_reps_all_levels(0)
  for (k in 1:max(label_rep_each_site_df$min_repeats)) {
    num_sites_with_k_reps_all_levels <- num_sites_with_k_reps_all_levels %>%
      left_join(get_num_sites_with_k_reps_all_levels(k), by = "grouping_var")
  }
  
  
  num_sites_with_k_reps_all_levels <- num_sites_with_k_reps_all_levels %>% 
    dplyr::mutate(label = "level.all_levels") %>%
    select(.data$grouping_var, .data$label, everything())
  
  
  num_label_repeats_obj <- rbind(num_label_repeats_obj, num_sites_with_k_reps_all_levels)
  
  
   
 # do some clean up to make the returned data frame more informative
  
  
  # if no grouping variable, remove the column for it, otherwise rename it back to the site_info name
  if (is.null(site_info_grouping_name)) {
    
    num_label_repeats_obj <- num_label_repeats_obj %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$grouping_var)
    
  } else {
    
    col_names <- names(num_label_repeats_obj)
    # add prefix "site_info" back to grouping name if it was not specified to make it
    #  clear that this variable corresponds to a site_info variable
    site_info_grouping_name <- ifelse(grepl("^site_info.", site_info_grouping_name),
                                      site_info_grouping_name,
                                      paste0("site_info.", site_info_grouping_name))
    col_names[which(col_names == "grouping_var")] <- site_info_grouping_name
    names(num_label_repeats_obj) <- col_names
  }
  
  
  # remove the word "label" from the level names
  num_label_repeats_obj$label <- gsub("^level.", "", num_label_repeats_obj$label)
  
  
  # replace the column named "label" with the actual label name
  col_names <- names(num_label_repeats_obj)
  col_names[which(col_names == "label")] <- paste0("labels.", labels)
  names(num_label_repeats_obj) <- col_names
  
  
  # rename "num_label_repeats_obj"to "num_label_repeats_obj"
  
  
  # making this an S3 object so that it can have an associated plot() function
  attr(num_label_repeats_obj, "class") <- c("label_repetition", "data.frame")
  attr(num_label_repeats_obj, "variable_used") <- labels
  
  num_label_repeats_obj
  
  
}






#' A plot function for label_repetition object
#'
#' This function plots the number of sites have at least k label repetitions.
#' Creating this plot is useful for assessing how to set the number of
#' cross-validation splits (and repeats of labels per cross-validation split) to
#' use in a datasource. This function returns a ggplot2 object which can be
#' further modified as needed.
#'
#' @param x A label_repetition object that was generated from calling the
#'   get_num_label_repetitions() function.
#' 
#' @param ... This is needed to conform to the plot generic interface.
#'
#' @param show_legend A Boolean specifying whether to show a legend 
#' that list which label each color in the plot corresponds to. 
#' 
#' @return. Returns a ggplot object that plots the number of sites that
#' that have at least k label repetitions as a function of k.  
#' 
#' 
#' @export
plot.label_repetition <- function(x, ..., show_legend = TRUE) {

  
  # if there is a variable "site_info" will facet the plot by this variable
  if (sum(grepl("^site_info", names(x)))) {
      
    site_info_facet_name <- names(dplyr::select(x, starts_with("site_info")))
    
    # rename the grouping_var and labels variables to make it easier for ggplot
    num_label_repeats_obj <- dplyr::rename(x, 
                                           label = starts_with("labels"),
                                           grouping_var = starts_with("site_info"))  
      
  } else {
    
    # put in a dummy grouping_var that will not be used
    num_label_repeats_obj <- dplyr::rename(x, label = starts_with("labels"))
    num_label_repeats_obj$grouping_var <- 1
    site_info_facet_name <- NULL
  }

  
  
  # extract label name that we have info about the number of repetitions
  variable_used <- attr(x, "variable_used")


  # pivot_longer() is giving an error message here so sticking with gather()
  #num_label_repeats_obj_long <- num_label_repeats_obj %>%
  #  tidyr::pivot_longer(-starts_with("grouping_var", "label")) %>%
  #  mutate(min_reps = as.numeric(.data$min_reps))

  num_label_repeats_obj_long <- num_label_repeats_obj %>%
    tidyr::gather("min_reps", "num_sites", -.data$label, -.data$grouping_var) %>%
    dplyr::mutate(min_reps = as.numeric(.data$min_reps)) %>%
    dplyr::mutate(line_type = ifelse(.data$label != "all_levels", "dotted", "solid"))
  
  
  # set breaks at integer increments 
  # if more than 20 labels, x-axis lines at every other value, etc.
  # could use the scales package for this...
  break_increments <- max(round(max(num_label_repeats_obj_long$min_reps)/20), 1)
  break_lines <- seq(0, max(num_label_repeats_obj_long$min_reps), by = break_increments)
  
  # set the color palette so that the "all-levels" color is black to make it stand out
  color_palette <- scales::hue_pal()(length(unique(num_label_repeats_obj_long$label)))
  color_palette[which(sort(unique(num_label_repeats_obj_long$label)) == "all_levels")] <- "#000000"

 
  # just plot the minimum number of repeats
  g <- num_label_repeats_obj_long %>%
    dplyr::filter(.data$label != "min_repeats") %>%
    dplyr::mutate(label = gsub("^level.", "", .data$label)) %>%
    ggplot(aes(.data$min_reps, .data$num_sites, col = .data$label, linetype = .data$line_type)) +
    geom_line() +
    geom_point() +
    xlab("Number of repeated conditions") +
    ylab("Number of sites") +
    ggtitle(paste("Label:", variable_used)) + 
    scale_x_continuous(breaks= break_lines) + 
    theme(panel.grid.minor.x = element_blank())  +  # put break lines at integer values
    guides(linetype = "none") + 
    scale_color_manual(values = color_palette)
  
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







#' @title Get the sitesIDs that have at least k trials for all label level
#' 
#' @description This function gets the siteIDs that have at least k label level
#'   repetitions. These siteIDs can be used in a datasource to only get data
#'   from sites that have enough label repetitions. For example, one could use
#'   these siteIDs in conjunction with the ds_basic's site_IDs_to_use argument
#'   to only get data from sites that have enough repetitions of each stimulus.
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param labels A string specifying which label variable should be
#'   used when calculating the minimum number of level repetitions.
#'   
#' @param k A number specifying that all sitesIDs returned should have at least k
#'   repetitions of all label levels.
#'   
#' @param label_levels A character vector specifying which levels to include.
#'   If not set, all levels will be used.
#'   
#' @return A vector of integers that specific which siteIDs have at least k
#'   repetitions of each label level (from the label levels that are used).
#'
#' @examples
#' data_file <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NeuroDecodeR")
#' get_siteIDs_with_k_label_repetitions(data_file, "stimulus_ID", 5)
#'
#'
#' @export
get_siteIDs_with_k_label_repetitions <- function(binned_data,
                                                 labels,
                                                 k,
                                                 label_levels = NULL) {
  
  
  siteIDs <- get_num_label_repetitions_each_site(binned_data, labels, label_levels) %>%
    dplyr::filter(.data$min_repeats >= k) %>%
    dplyr::pull(.data$siteID)
  
  
  siteIDs

}








#' @title Get the number of trial repetitions for a given label for each site
#'
#' @description Calculates how many repeated trials there are for each label
#' level for each site. This can be useful for selecting sites that have a
#' minimum number of repetitions of each stimulus or other experimental
#' condition.
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param labels A string specifying which label variable should be
#'   used for calculating the minimum number of level repetitions.
#'
#' @param label_levels A character vector specifying which levels to include.
#' If not set, all levels will be used.
#' 
#' @return A data frame where each row corresponds to a recording site. The columns
#' in the data frame are:
#'
#'   * \emph{siteID}: The siteID each row in the data frame corresponds to
#'   * \emph{min_repeats}: minimum number of repeats across all label levels
#'   * \emph{level_XXX}: The number or repeats for a specific label level 
#'   * \emph{site_info.XXX}: The site_info for each site
#'  
#'
get_num_label_repetitions_each_site <- function(binned_data,
                                      labels,
                                      label_levels = NULL) {
  
  
  # load data if a string is given and check that the data is in binned format
  binned_data <- check_and_load_binned_data(binned_data)
  
  
  # get the site_info which will be returned along with num label repetitions 
  #  to allow additional filtering
  site_info <- binned_data %>%
    select("siteID", starts_with("site_info")) 
  
  
  # select only the siteID and labels we want to count number of repetitions
  binned_data <- select(binned_data, .data$siteID, label = paste0("labels.", labels))
  

    # make sure that the labels are a factors
  binned_data <- binned_data %>%
    mutate(label = as.factor(.data$label))
  
  
  if (is.null(label_levels)) {
    label_levels <- levels(binned_data$label)
  }
 
  # test label_levels specified are all levels that exist
  invalid_levels_specified <- setdiff(label_levels, levels(binned_data$label))
  if (length(invalid_levels_specified) != 0) {
    stop(paste(
      "The level(s)", paste0("'", paste(invalid_levels_specified, collapse = ", "), "'"),
      "do not exist in the variable", labels))
  }
  
  
  # reduce data to only the levels specified
  binned_data <- dplyr::filter(binned_data, binned_data$label %in% label_levels)
  
  num_repeats_per_level <- binned_data %>%
    dplyr::group_by(.data$siteID, .data$label) %>%
    count() %>%
    dplyr::mutate(label = paste0("level.", .data$label))
  
  # if some label levels don't exist in the data, add them with a value of n = 0
  zero_n_repeats_df <- data.frame(siteID = rep(unique(num_repeats_per_level$siteID), each = length(label_levels)),
                                  label = rep(paste0("level.", label_levels), length(unique(num_repeats_per_level$siteID))),
                                  n = 0)
  
  num_repeats_per_level <- rbind(num_repeats_per_level, zero_n_repeats_df) %>%
    group_by(.data$siteID, .data$label) %>%
    summarize(n = max(n))
    
  
  # get the value of the level that has the minimum repetitions
  min_num_repeats_per_level <- num_repeats_per_level %>%
    dplyr::group_by(.data$siteID) %>%
    dplyr::summarize(min_repeats = min(n)) 
  
  
  label_rep_each_site_df <- tidyr::pivot_wider(num_repeats_per_level, 
                                      names_from = .data$label, 
                                      values_from = n) %>%
    dplyr::full_join(min_num_repeats_per_level, by = "siteID") %>%
    dplyr::select(.data$siteID, .data$min_repeats, everything())  
  
  
  
  
  # add the site_info to the label_rep_each_site_df if site_info variables exist
  
  if ((site_info %>% select(-.data$siteID) %>% ncol()) != 0) {

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
    get_first <- function(df) df[1]
    site_info_to_use <- site_info %>%
      dplyr::select("siteID", all_of(site_info_variables_names_to_include)) %>%
      dplyr::group_by(.data$siteID) %>%
      summarize(across(everything(), get_first))
    
    # dplyr::first() doesn't work when vectors are nested in data frames, 
    # so using get_first() function instead
    ##  dplyr::summarize(across(everything(), dplyr::first)) 
    
    
    label_rep_each_site_df <- left_join(label_rep_each_site_df, site_info_to_use, "siteID")
    
  }

  
  label_rep_each_site_df
  
}





