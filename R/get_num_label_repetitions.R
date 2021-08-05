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
    group_by(.data$siteID, .data$label) %>%
    count()

  min_num_repeats_per_level <- num_repeats_per_level %>%
    group_by(.data$siteID) %>%
    summarize(min_repeats = min(n))

  label_rep_obj <- tidyr::spread(num_repeats_per_level, .data$label, n) %>%
    full_join(min_num_repeats_per_level, by = "siteID") %>%
    select(.data$siteID, .data$min_repeats, everything())


  # making this an S3 object so that it can have an associated plot() function
  attr(label_rep_obj, "class") <- c("label_repetition", "data.frame")
  attr(label_rep_obj, "variable_used") <- variable_to_use


  label_rep_obj

}




#' A plot function for label_repetition object
#'
#' This function plots how many label repetitions there are for each stimulus.
#' It is useful for assessing how to set the number of cross-validation splits
#' (and repeats of labels per cross-validation split) in a datasource. This
#' function returns a ggplot2 object which can be further modified as needed.
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
#'
#' @export
plot.label_repetition <- function(x, ..., show_legend = TRUE) {

  label_rep_obj <- x

  num_sites_with_k_repeats <- get_num_sites_with_k_label_repetitions(label_rep_obj)

  num_sites_with_k_repeats_long <- num_sites_with_k_repeats %>%
    tidyr::gather("min_reps", "num_sites", -.data$label) %>%
    mutate(min_reps = as.numeric(.data$min_reps))

  
  # set breaks at integer increments 
  # if more than 20 labels, x-axis lines at every other value, etc.
  break_increments <- max(round(max(label_rep_info$min_repeats)/20), 1)
  break_lines <- seq(0, max(label_rep_info$min_repeats), by = break_increments)
  
  
  # just plot the minimum number of repeats
  g <- num_sites_with_k_repeats_long %>%
    dplyr::filter(.data$label != "min_repeats") %>%
    ggplot(aes(.data$min_reps, .data$num_sites, col = .data$label)) +
    geom_line() +
    geom_point() +
    xlab("Number of repeated conditions") +
    ylab("Number of sites") +
    ggtitle(paste("Label:", attr(label_rep_obj, "variable_used"))) + 
    scale_x_continuous(breaks= break_lines) + 
    theme(panel.grid.minor.x = element_blank())   # put break lines at integer values

  
  # argument to turn off the legends
  # could make this the default if there are too many labels...
  if (show_legend == FALSE) {
    g <- g + 
      theme(legend.position = "none")
  }
  
  
  g

}



# a helper function that returns how many sites have k repeats for all labels
# this is used above to plot the data
get_num_sites_with_k_label_repetitions <- function(label_rep_obj) {

  max_min_num_reps <- max(label_rep_obj$min_repeats)

  label_rep_long <- tidyr::gather(label_rep_obj, "label", "num_reps", -.data$siteID)

  num_sites_with_k_repeats <- label_rep_long %>%
    group_by(.data$label) %>%
    summarize("0" = sum(.data$num_reps >= 0))

  for (k in 1:max_min_num_reps) {

    curr_count <- label_rep_long %>%
      group_by(.data$label) %>%
      summarize(num_with_k_reps = sum(.data$num_reps >= k)) %>%
      select(.data$num_with_k_reps)

    names(curr_count) <- as.character(k)      # paste0("reps_", sprintf('%03d', k))

    num_sites_with_k_repeats <- cbind(num_sites_with_k_repeats, curr_count)
    
  }


  num_sites_with_k_repeats

}
