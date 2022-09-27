#' A basic datasource (DS)
#'
#' The standard datasource used to get training and test splits of data.
#'
#' This 'basic' datasource is the datasource that will most commonly be used for
#' most analyses. It can generate training and tests sets for data that has been
#' recorded simultaneously or pseudo-populations for data that was not recorded
#' simultaneously.
#'
#' Like all datasources, this datasource takes binned format data and has a
#' `get_data()` method that is never explicitly called by the user of the
#' package, but rather it is called internally by a cross-validation object to
#' get training and testing splits of data that can be passed to a classifier.
#' 
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param labels A string specifying the name of the labels that should
#'   be decoded. This label must be one of the columns in the binned data that
#'   starts with 'label.'. For example, if there was a column name in a binned
#'   data file called labels.stimulus_ID that you wanted to decode, then you
#'   would set this argument to be "stimulus_ID".
#'
#' @param num_cv_splits A number specifying how many cross-validation splits
#'  should be used.
#'
#' @param use_count_data If the binned data is neural spike counts, then setting
#'   use_count_data = TRUE will convert the data into spike counts. This is
#'   useful for classifiers that work on spike count data, e.g., the
#'   poisson_naive_bayes_CL.
#'
#' @param num_label_repeats_per_cv_split A number specifying how many times each
#'   label should be repeated in each cross-validation split.
#'
#' @param label_levels A vector of strings specifying specific label
#'   levels that should be used. If this is set to NULL then all label levels
#'   available will be used.
#'
#' @param num_resample_sites The number of sites that should be randomly
#'   selected when constructing training and test vectors. This number needs to
#'   be less than or equal to the number of sites available that have
#'   num_cv_splits * num_label_repeats_per_cv_split repeats.
#'
#' @param site_IDs_to_use A vector of integers specifying which sites should be
#'   used. If this is NULL (default value), then all sites that have 
#'   num_cv_splits * num_label_repeats_per_cv_split repeats will be used, and 
#'   a message about how many sites are used will be displayed.
#'
#' @param site_IDs_to_exclude A vector of integers specifying which sites should
#'   be excluded.
#'
#' @param randomly_shuffled_labels A Boolean specifying whether the labels
#'   should be shuffled prior to running an analysis (i.e., prior to the first
#'   call to the the get_data() method). This is used when one wants to create a
#'   null distribution for comparing when decoding results are above chance.
#'
#' @param create_simultaneous_populations If the data from all sites
#'   was recorded simultaneously, then setting this variable to 1 will cause the
#'   get_data() function to return simultaneous populations rather than
#'   pseudo-populations.
#'
#' @return This constructor creates an NDR datasource object with the class
#'   `ds_basic`. Like all NDR datasource objects, this datasource will be used
#'   by the cross-validator to generate training and test data sets.
#'
#'
#' @examples
#' # A typical example of creating a datasource to be passed cross-validation object
#' data_file <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NeuroDecodeR")
#' ds <- ds_basic(data_file, "stimulus_ID", 18)
#'
#' # If one has many repeats of each label, decoding can be faster if one
#' # uses fewer CV splits and repeats each label multiple times in each split.
#' ds <- ds_basic(data_file, "stimulus_ID", 6,
#'   num_label_repeats_per_cv_split = 3
#' )
#'
#' # One can specify a subset of labels levels to be used in decoding. Here
#' #  we just do a three-way decoding analysis between "car", "hand" and "kiwi".
#' ds <- ds_basic(data_file, "stimulus_ID", 18,
#'   label_levels = c("car", "hand", "kiwi")
#' )
#'
#' # One never explicitly calls the get_data() function, but rather this is
#' # called by the cross-validator. However, to illustrate what this function
#' # does, we can call it explicitly here to get training and test data:
#' all_cv_data <- get_data(ds)
#' names(all_cv_data)
#'
#' @family datasource
#'


# the constructor
#' @export
ds_basic <- function(binned_data,
                     labels,
                     num_cv_splits,
                     use_count_data = FALSE,
                     num_label_repeats_per_cv_split = 1,
                     label_levels = NULL,
                     num_resample_sites = NULL,
                     site_IDs_to_use = NULL,
                     site_IDs_to_exclude = NULL,
                     randomly_shuffled_labels = FALSE,
                     create_simultaneous_populations = 0) {

  if (is.character(binned_data)) {
    binned_file_name <- binned_data
  } else {
    binned_file_name <- "Loaded binned data given"
  }

  # load data if a string is given and check that the data is in binned format
  binned_data <- check_and_load_binned_data(binned_data)

  if (use_count_data) {
    binned_data <- convert_rates_to_counts(binned_data)
  }

  # store the original binned data in order to get the site_IDs_to_use if it is null
  #  (a bit hacky and not memory efficient but should be ok)
  binned_data_org <- binned_data
  

  # remove all labels that aren't being used, and rename the labels that are being used to "labels"
  label_col_ind <- match(paste0("labels.", labels), names(binned_data))

  # also keep the variable trial_number if it exists
  if (("trial_number" %in% colnames(binned_data))) {
    binned_data <- binned_data %>%
      dplyr::select(.data$siteID, starts_with("time"), .data$trial_number, labels = label_col_ind)
  } else {
    binned_data <- binned_data %>%
      dplyr::select(.data$siteID, starts_with("time"), labels = label_col_ind)
  }


  # only use specified label_levels
  if (!is.null(label_levels)) {
    binned_data <- dplyr::filter(binned_data, labels %in% label_levels)
  } else {
    label_levels <- as.list(levels(binned_data$labels))  # why is this a list? (b/c of ds_generalization???)
  }


  # if (is.null(site_IDs_to_use)) {
  #  site_IDs_to_use <- unique(binned_data$siteID)
  #}

  if (is.null(site_IDs_to_use)) {
    
    # if site_IDs_to_use is not specified, use all sites that have enough label repetitions
    site_IDs_to_use <- get_siteIDs_with_k_label_repetitions(binned_data_org, 
                                                            labels,
                                                            k = num_cv_splits * num_label_repeats_per_cv_split,
                                                            label_levels = unlist(label_levels))
    # print message about which sites are used
    message(
      paste0("Automatically selecting sites_IDs_to_use.",
             " Since num_cv_splits = ", num_cv_splits, 
             " and num_label_repeats_per_cv_split = ", num_label_repeats_per_cv_split,
             ", all sites that have ", num_cv_splits * num_label_repeats_per_cv_split, 
             " repetitions have been selected. This yields ", length(site_IDs_to_use),
             " sites that will be used for decoding (out of ", length(unique(binned_data$siteID)),
             " total).")
      )
    
    
    # Give an error message if no sites are available for decoding
    # Could give an error if there are not at least 2 sites available, but will allow one site for now
    if (length(site_IDs_to_use) < 1) {
      
      stop(
        paste("\nNo sites are available that enough trial repetitions based on",
              "the num_cv_splits, num_label_repeats_per_cv_split, and label_levels that were specified.", 
              "Please use different values for these parameters, and/or manually specify the site_IDs_to_use.")
        )
      
    }
    
    
    # message(site_IDs_to_use)
    
    # free up some memory since this is not used elsewhere
    rm(binned_data_org)
    
  }
  
  
  if (!is.null(site_IDs_to_exclude)) {
    site_IDs_to_use <- setdiff(site_IDs_to_use, site_IDs_to_exclude)
  }

  if (length(label_levels) != length(unique(label_levels))) {
    warning("Some labels were listed twice. Duplication will be ignored.")
  }

  if (is.null(num_resample_sites)) {
    num_resample_sites <- length(site_IDs_to_use)
  }

  if (create_simultaneous_populations > 2 || create_simultaneous_populations < 0) {
    stop("create_simultaneous_populations must be set to 0 or 1.")
  }


  # check if data is valid to get simultaneously recorded data
  if (create_simultaneous_populations == 1 || create_simultaneous_populations == TRUE) {

    # for simultaneously recorded data there should be the same number of labels for each site
    num_trials_for_each_label_for_each_site <- binned_data %>%
      dplyr::group_by(.data$siteID, labels) %>%
      dplyr::summarize(n = n()) %>% 
      tidyr::pivot_wider(names_from = .data$labels, values_from = .data$n) 

    # for some reason select(-.data$siteID) isn't working
    num_trials_for_each_label_for_each_site$siteID <- NULL
    
    if (sum(sapply(lapply(num_trials_for_each_label_for_each_site, unique), length) != 1)) {
      stop(paste(
        "There are not the same number of repeated labels/trials for each site which",
        "which there should be for simultaneously recorded data."
      ))
    }


    # add a variable called 'trial_number' if it doesn't exist and the data was recorded simultaneously
    if (!("trial_number" %in% colnames(binned_data))) {

      warning(paste(
        "No variable named trial_number in the binned_data.\n",
        "Attempting to add this variable to decode simultaneously recorded data",
        "by assuming all trials for each site are in the same sequential order."
      ))

      num_trials_each_site <- binned_data %>%
        dplyr::group_by(.data$siteID) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::select(.data$n)

      # assuming the trials are in order for each site, otherwise there is no way to align them
      binned_data$trial_number <- rep(1:num_trials_each_site$n[1], dim(num_trials_each_site)[1])

      binned_data <- dplyr::select(binned_data, .data$siteID, .data$trial_number, everything())

    }



    # shuffle the labels if specified (shuffle labels the same way for each site)
    if (randomly_shuffled_labels == TRUE) {

      min_site_ID <- min(binned_data$siteID)
      first_site_data <- dplyr::filter(binned_data, .data$siteID == min_site_ID)
      the_labels <- sample(first_site_data$labels)
      binned_data$labels <- rep(the_labels, length(unique(binned_data$siteID)))

    }


    # add variable label_trial_combo
    binned_data <- binned_data %>%
      mutate(label_trial_combo = paste0(binned_data$labels, "_", binned_data$trial_number))


    # end pre-processing for simultaneously recorded data...
  } else {

    # shuffle the labels if specified
    if (randomly_shuffled_labels == TRUE) {
      binned_data <- binned_data %>%
        ungroup() %>%
        group_by(.data$siteID) %>%
        mutate(labels = labels[sample(row_number())]) %>%
        ungroup()
    }
  }


  # create the main data structure
  the_ds <- list(
    binned_file_name = binned_file_name,
    binned_data = binned_data,
    labels = labels,
    num_cv_splits = num_cv_splits,
    num_label_repeats_per_cv_split = num_label_repeats_per_cv_split,
    label_levels = label_levels,
    num_resample_sites = num_resample_sites,
    site_IDs_to_use = site_IDs_to_use,
    site_IDs_to_exclude = site_IDs_to_exclude,
    randomly_shuffled_labels = randomly_shuffled_labels,
    create_simultaneous_populations = create_simultaneous_populations
  )


  attr(the_ds, "class") <- "ds_basic"
  the_ds

}




#' @inherit get_data
#' @keywords internal
#' @export
get_data.ds_basic <- function(ds_obj) {

  binned_data <- ds_obj$binned_data
  num_cv_splits <- ds_obj$num_cv_splits
  num_trials_used_per_label <- ds_obj$num_cv_splits * ds_obj$num_label_repeats_per_cv_split

  create_simultaneous_populations <- ds_obj$create_simultaneous_populations
  num_resample_sites <- ds_obj$num_resample_sites
  site_IDs_to_use <- ds_obj$site_IDs_to_use

  create_simultaneous_populations <- ds_obj$create_simultaneous_populations

  num_label_repeats_per_cv_split <- ds_obj$num_label_repeats_per_cv_split



  # the code that actually gets the data used to train and test the classifier  -----------

  curr_sites_to_use <- sample(site_IDs_to_use, num_resample_sites)
  binned_data <- dplyr::filter(binned_data, .data$siteID %in% curr_sites_to_use)


  if (create_simultaneous_populations == 1) {

    # use one site to select the trials to use and then apply to all sites
    curr_label_trials_to_use <- binned_data %>%
      dplyr::filter(.data$siteID == binned_data$siteID[1]) %>%
      select(labels, .data$label_trial_combo) %>%
      group_by(labels) %>%
      sample_n(size = num_trials_used_per_label) %>%
      pull(.data$label_trial_combo)

    # apply specific simultaneous trials selected to all sites
    all_k_fold_data <- binned_data %>%
      dplyr::filter(.data$label_trial_combo %in% curr_label_trials_to_use) %>%
      dplyr::mutate(label_trial_siteID_combo = paste0(.data$label_trial_combo, '_', .data$siteID))

    
    # Arrange rows for each site of all_k_fold_data to be in the random order
    # specified by curr_label_trials_to_use. This ensures a different random
    # ordering of data each time get_data() is called.
    curr_label_trials_to_use_siteID <- paste0(curr_label_trials_to_use, '_', 
                                              all_k_fold_data$siteID)
    
    all_k_fold_data <- all_k_fold_data[match(curr_label_trials_to_use_siteID, 
                                             all_k_fold_data$label_trial_siteID_combo), ]
   
    all_k_fold_data <- all_k_fold_data %>% 
      dplyr::select(-.data$label_trial_combo, -.data$label_trial_siteID_combo)
    
                                              

  } else {

    # for data not recorded simultaneously
    all_k_fold_data <- binned_data %>%
      group_by(labels, .data$siteID) %>%
      sample_n(size = num_trials_used_per_label)
  }


  # remove the variable trial_number if it exists in all_k_fold_data
  if ("trial_number" %in% names(all_k_fold_data)) {
    all_k_fold_data <- select(all_k_fold_data, -.data$trial_number)
  }


  unique_labels <- unique(all_k_fold_data$labels)
  num_sites <- length(unique(binned_data$siteID))
  num_labels <- length(unique_labels)


  # arrange the data by siteID and labels before adding on the CV_slide_ID
  all_k_fold_data <- dplyr::arrange(all_k_fold_data, .data$siteID, labels)

  
  # CV_slice_ID is a groups of data that have one example for each label
  #  - these groups are mapped into CV blocks where blocks contain num_label_repeats_per_cv_split of each label
  CV_slice_ID <- rep(1:num_trials_used_per_label, num_labels * num_sites)

  # add the number of the cross-validation split to the data
  all_k_fold_data$CV_slice_ID <- CV_slice_ID

  # paste the site.000 in front of the siteID so that is is listed as site_0001, site_0002, etc
  all_k_fold_data$siteID <- paste0("site_", stringr::str_pad(all_k_fold_data$siteID, 4, pad = "0"))

  
  # convert so that there are one column for each site
  
  # older version that uses gather/spread
  #melted_data <- tidyr::gather(all_k_fold_data, "time_bin", "activity", -.data$siteID, -labels, -CV_slice_ID)
  #all_cv_data_old <- tidyr::spread(melted_data, .data$siteID, .data$activity) %>%
  #  select(labels, .data$time_bin, CV_slice_ID, everything()) %>%
  #  mutate(time_bin = as.factor(.data$time_bin)) #  %>%  arrange(labels, time_bin)

  
  long_data <- tidyr::pivot_longer(all_k_fold_data, 
                                   -c(.data$siteID, .data$labels, .data$CV_slice_ID),
                                   names_to = "time_bin", 
                                   values_to = "activity")

   all_cv_data <- tidyr::pivot_wider(long_data, 
                                    names_from = .data$siteID, 
                                    values_from = .data$activity) %>%
    select(labels, .data$time_bin, CV_slice_ID, everything()) %>%
    mutate(time_bin = as.factor(.data$time_bin)) #  %>%  arrange(labels, time_bin)
  

   
  # create different CV_1, CV_2 which list which points are training points and which points are test points
  for (iCV in 1:num_cv_splits) {
    start_ind <- (((iCV - 1) * num_label_repeats_per_cv_split) + 1)
    end_ind <- (iCV * num_label_repeats_per_cv_split)
    curr_cv_block_inds <- start_ind:end_ind
    eval(parse(text = paste0("all_cv_data$CV_", iCV, "= ifelse(all_cv_data$CV_slice_ID %in% curr_cv_block_inds, 'test', 'train')")))
  }


  all_cv_data <- dplyr::select(all_cv_data, -CV_slice_ID) %>%
    dplyr::ungroup() # fails tests if I don't ungroup. Also remove the original CV_slice_ID field


  # add train_labels and test_labels columns
  all_cv_data <- all_cv_data %>%
    mutate(train_labels = labels) %>%
    rename(test_labels = labels) %>%
    select(.data$train_labels, .data$test_labels, everything())


  all_cv_data

} # end get_data()






#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.ds_basic <- function(ndr_obj) {

  ndr_obj$binned_data <- NULL

  variable_lengths <- sapply(ndr_obj, length)
  length_one_variables <- variable_lengths[variable_lengths < 2]
  length_one_variables <- ndr_obj[names(length_one_variables)]

  # convert null values to NAs so that the variables are retained
  length_one_variables <- sapply(length_one_variables, function(x) ifelse(is.null(x), NA, x))

  parameter_df <- data.frame(val = unlist(length_one_variables)) %>%
    tibble::rownames_to_column("key") %>%
    tidyr::spread("key", "val") %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  parameter_df$label_levels <- list(sort(unlist(ndr_obj$label_levels)))
  parameter_df$site_IDs_to_use <- list(ndr_obj$site_IDs_to_use)

  names(parameter_df) <- paste(class(ndr_obj), names(parameter_df), sep = ".")

  parameter_df

}
