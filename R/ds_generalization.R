#' A datasource that allows one and test on different but related labels
#'
#' This datasource is useful for assessing whether information is
#' invariant/abstract to particular conditions.
#' 
#' Like all datasources, this datasource takes binned format data
#' and has a get_data() method that is called by a cross-validation object to 
#' get training and testing splits of data that can be passed to a classifier. 
#'
#'
#' @param binned_file_name A string with the name of a file that has 
#'  data in binned format.
#' 
#' @param var_to_decode A string specifying the name of the labels that
#'  should be decoded. This label must be one of the columns in the binned
#'  data that starts with 'label.' 
#'  
#' @param num_cv_splits A number specifying how many cross-validation splits
#'  should be used. 
#'  
#' @param train_label_levels A list...
#'
#' @param test_label_levels A list...
#'   
#' @param use_count_data If the binned data is neural spike counts, then setting
#'   use_count_data = TRUE will convert the data into spike counts. This is
#'   useful for classifiers that work on spike count data, e.g., the
#'   poisson_naive_bayes_CL.
#' 
#' @param num_label_repeats_per_cv_split A number specifying how many times each
#'   label should be repeated in each cross-validation split.
#' 
#' 
#' @param num_resample_sites The number of sites that should be randomly
#'   selected when constructing training and test vectors. This number needs to
#'   be less than or equal to the number of sites available that have
#'   num_cv_splits * num_label_repeats_per_cv_split repeats.
#'   
#' @param site_IDs_to_use A vector of integers specifying which sites should be
#'   used.
#'
#' @param site_IDs_to_exclude A vector of integers specfying which sites should
#'   be excluded.
#' 
#' @param randomly_shuffled_labels_before_running A boolean specifying whether
#'   the labels should be shuffled prior to the get_data() function being
#'   called. This is used when one wants to create a null distribution for
#'   comparing when decoding results are above chance.
#' 
#' @param create_simultaneously_recorded_populations If the data from all sites
#'   was recorded simultaneously, then setting this variable to 1 will cause the
#'   get_data() function to return simultaneous populations rather than
#'   pseudo-populations.
#' 
#' 
#' @examples
#'  # A typical example of creating a datasource to be passed cross-validation object   
#'  binned_file_name <- file.path('data', 'binned', 'ZD_150_samples_binned_every_50_samples.Rda')
#'  ds <- ds_basic(binned_file_name, 'stimulus_ID', 18)
#'  
#'  # If one has many repeats of each label, decoding can be faster if one
#'  # uses fewer CV splits and repeats each label multiple times in each split.
#'  ds <- ds_basic(binned_file_name, 'stimulus_ID', 6,
#'                 num_label_repeats_per_cv_split = 3)
#'  
#'  # One can specify a subset of labels levels to be used in decoding. Here
#'  #  we just do a three-way decoding analysis between "car", "hand" and "kiwi".
#'  ds <- ds_basic(binned_file_name, 'stimulus_ID', 18,
#'                 label_levels_to_use = c("car", "hand", "kiwi")) 
#'  
#'  # One never explicitely calls the get_data() function, but rather this is
#'  # done by the cross-validator. However, to illustrate what this function
#'  # does, we can call it explicitly here to get training and test data:
#'  cv_data <- get_data(ds)  
#'  names(cv_data)
#' 
#'  
#' 
#' @family datasource



#' @export
ds_generalization <- function(binned_file_name, 
                            var_to_decode, 
                            num_cv_splits,
                            train_label_levels,
                            test_label_levels,
                            use_count_data = FALSE,
                            num_label_repeats_per_cv_split = 1, 
                            num_resample_sites = NULL,
                            site_IDs_to_use = NULL,
                            site_IDs_to_exclude = NULL,
                            randomly_shuffled_labels_before_running = FALSE,
                            create_simultaneously_recorded_populations = 0) {
  
  
  # construct a ds_basic object that will do most of the work
  
  the_basic_ds
    
    
  

  # load the binned data and convert it to spike counts if specified
  load(binned_file_name)
  
  if (use_count_data) {
    binned_data <- convert_rates_to_counts(binned_data) 
  }
  
  
  # remove all labels that aren't being used, and rename the labels that are being used "labels"
  label_col_ind <- match(paste0("labels.", var_to_decode), names(binned_data))
  
  # also keep the variable trial_number if it exists
  if (("trial_number" %in% colnames(binned_data))) {
    binned_data <- binned_data %>% 
      dplyr::select(siteID, starts_with("time"), trial_number, labels = label_col_ind)
  } else {
    binned_data <- binned_data %>% 
      dplyr::select(siteID, starts_with("time"), labels = label_col_ind)
  }
  
  
  # only use specified label_levels 
  if(!is.null(label_levels_to_use)) {
    print(label_levels_to_use)
    binned_data <- dplyr::filter(binned_data, labels %in% label_levels_to_use)
  } else {
    label_levels_to_use <- as.list(levels(binned_data$labels))
  }
  
  
  # shuffle the labels if specified
  if(randomly_shuffled_labels_before_running == TRUE) {
    binned_data$labels <- sample(binned_data$labels)
  }
    
  
  if(is.null(site_IDs_to_use)) {
    site_IDs_to_use <- unique(binned_data$siteID)
  }
  
  if(!is.null(site_IDs_to_exclude)) {
    site_IDs_to_use <- setdiff(site_IDs_to_use, site_IDs_to_exclude)
  }
  
  if(length(label_levels_to_use) != length(unique(label_levels_to_use)))
    warning("Some labels were listed twice. Duplication will be ignored.")
  
  
  if(is.null(num_resample_sites)) {
    num_resample_sites <- length(site_IDs_to_use)
  }
  
  
  if(create_simultaneously_recorded_populations > 2 || create_simultaneously_recorded_populations < 0)
    stop("create_simultaneously_recorded_populations must be set to 0 or 1.")
  
  
  # check if data is valid to get simultaneously recorded data
  if(create_simultaneously_recorded_populations == 1) {

    # for simultaneously recorded data there should be the same number of labels for each site
    num_trials_for_each_label_for_each_site <- binned_data %>%
      dplyr::group_by(siteID, labels) %>%
      dplyr::summarize(n = n()) 
    
    if (length(unique(num_trials_for_each_label_for_each_site$n)) != 1) {
      stop(paste('There are not the same number of repeated labels/trials for each site which',
                 'which there should be for simultaneously recorded data.'))
    }
    
    
    # add a variable called 'trial_number' if it doesn't exist and the data was recorded simultaneously
    if (!("trial_number" %in% colnames(binned_data))) {
      
      warning(paste('No variable named trial_number in the binned_data.\n',
                    'Attempting to add this variable to decode simultaneously recorded data',
                    'by assuming all trials for each site are in the sequential same order.'))

      num_trials_each_site <- binned_data %>%
        dplyr::group_by(siteID) %>%
        dplyr::summarize(n = n()) %>%
        .$n
      
      # assuming the trials are in order for each site, otherwise there is no way to align them
      binned_data$trial_number <- rep(1:num_trials_each_site[1], length(num_trials_each_site)) 
      
      binned_data <- dplyr::select(binned_data, siteID, trial_number, everything())
      
    }
    
    
    # add variable label_trial_combo
    binned_data <- binned_data  %>% 
      mutate(label_trial_combo = paste0(binned_data$labels, binned_data$trial_number))
    
  }  # end pre-processing for simultaneously recorded data...
  
  
  
  # create the main data structure
  the_ds <- list(
    binned_file_name = binned_file_name,
    binned_data = binned_data,
    var_to_decode = var_to_decode,
    num_cv_splits = num_cv_splits,
    num_label_repeats_per_cv_split = num_label_repeats_per_cv_split,
    label_levels_to_use = label_levels_to_use,
    num_resample_sites = num_resample_sites,
    site_IDs_to_use = site_IDs_to_use,
    site_IDs_to_exclude = site_IDs_to_exclude,
    randomly_shuffled_labels_before_running = randomly_shuffled_labels_before_running,
    create_simultaneously_recorded_populations = create_simultaneously_recorded_populations
  )
  
  
  attr(the_ds, "class") <- "ds_basic"
  the_ds
  
  
}
                      


      
get_data.ds_basic = function(ds_basic_obj){
        

    binned_data <- ds_basic_obj$binned_data
    var_to_decode <- ds_basic_obj$var_to_decode
    num_cv_splits <- ds_basic_obj$num_cv_splits
    num_trials_used_per_label <- ds_basic_obj$num_cv_splits * ds_basic_obj$num_label_repeats_per_cv_split
    label_levels_to_use <- ds_basic_obj$label_levels_to_use

    create_simultaneously_recorded_populations <- ds_basic_obj$create_simultaneously_recorded_populations
    sample_sites_with_replacement <- ds_basic_obj$sample_sites_with_replacement
    num_resample_sites <- ds_basic_obj$num_resample_sites
    site_IDs_to_use <- ds_basic_obj$site_IDs_to_use
    site_IDs_to_exclude <- ds_basic_obj$site_IDs_to_exclude

    create_simultaneously_recorded_populations <- ds_basic_obj$create_simultaneously_recorded_populations

    num_label_repeats_per_cv_split <- ds_basic_obj$num_label_repeats_per_cv_split


    
        
    # the code that actually gets the data used to train and test the classifier  -----------

    curr_sites_to_use <- sample(site_IDs_to_use, num_resample_sites)
    binned_data <- dplyr::filter(binned_data, siteID %in% curr_sites_to_use)


    if (create_simultaneously_recorded_populations == 1) {

      # use one site to select the trials to use and then apply to all sites
      curr_label_trials_to_use <- binned_data %>%
        filter(siteID == binned_data$siteID[1]) %>%
        select(labels, label_trial_combo) %>%
        group_by(labels) %>%
        sample_n(size = num_trials_used_per_label) %>%
        .$label_trial_combo

      # apply specific simultaneous trials selected to all sites
      all_k_fold_data <- binned_data  %>%
        filter(label_trial_combo %in% curr_label_trials_to_use) %>%
        select(-label_trial_combo)

    } else {

      # for data not recorded simultaneously
      all_k_fold_data <- binned_data  %>%
        group_by(labels, siteID) %>%
        sample_n(size = num_trials_used_per_label)
    }


    # remove the variable trial_number if it exists in all_k_fold_data
    if ("trial_number" %in% names(all_k_fold_data)) {
      all_k_fold_data <- select(all_k_fold_data, -trial_number)
    }


    unique_labels <- unique(all_k_fold_data$labels)
    num_sites <- length(unique(binned_data$siteID))
    num_time_bins <- sum(grepl("time_*", names(binned_data)))
    num_labels <- length(unique_labels)



    # CV_slice_ID is a groups of data that have one example for each label
    #  - these groups are mapped into CV blocks where blocks contain num_label_repeats_per_cv_split of each label
    CV_slice_ID <- rep(1:num_trials_used_per_label, num_labels * num_sites)

    # add the number of the cross-validitation split to the data
    all_k_fold_data$CV_slice_ID <- CV_slice_ID

    # paste the site.000 in front of the siteID so that is is listed as site_0001, site_0002, etc
    all_k_fold_data$siteID <- paste0("site_", stringr::str_pad(all_k_fold_data$siteID, 4, pad = "0"))

    # convert so that there are one column for each site
    melted_data <- tidyr::gather(all_k_fold_data, time_bin, activity, -siteID, -labels, -CV_slice_ID)
    all_cv_data <- tidyr::spread(melted_data, siteID, activity) %>%
      select(labels, time_bin, CV_slice_ID, everything()) %>%
      mutate(time_bin = as.factor(time_bin))    #  %>%  arrange(labels, time_bin)

    # create different CV_1, CV_2 which list which points are training points and which points are test points
    for (iCV in 1:num_cv_splits) {
      start_ind <- (((iCV - 1) * num_label_repeats_per_cv_split) + 1)
      end_ind <- (iCV * num_label_repeats_per_cv_split)
      curr_cv_block_inds <- start_ind:end_ind
      eval(parse(text=paste0("all_cv_data$CV_", iCV, "= ifelse(all_cv_data$CV_slice_ID %in% curr_cv_block_inds, 'test', 'train')")))
    }


    all_cv_data <- dplyr::select(all_cv_data, -CV_slice_ID) %>% dplyr::ungroup()  # fails tests if I don't ungroup. Also remove the original CV_slice_ID field


    return(all_cv_data)


}  # end get_data()




get_parameters.ds_basic = function(ds_basic_obj){

  ds_basic_obj$binned_data <- NULL
  
  variable_lengths <- sapply(ds_basic_obj, length)
  length_one_variables <- variable_lengths[variable_lengths < 2]
  length_one_variables <- ds_basic_obj[names(length_one_variables)]
  
  # convert null values to NAs so that the variables are retained
  length_one_variables <- sapply(length_one_variables, function(x) ifelse(is.null(x), NA, x))
  
  parameter_df <- data.frame(val = unlist(length_one_variables)) %>%
    mutate(key = rownames(.)) %>% 
    tidyr::spread(key, val) %>%
    mutate_all(type.convert) %>%
    mutate_if(is.factor, as.character)
  
  parameter_df$label_levels_to_use <- list(sort(unlist(ds_basic_obj$label_levels_to_use)))
  parameter_df$site_IDs_to_use <- list(ds_basic_obj$site_IDs_to_use)
  
  names(parameter_df) <- paste(class(ds_basic_obj), names(parameter_df), sep = ".")
  
  parameter_df

}


