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
  
  
  # check the same number of classes in the training and test set
  if(length(train_label_levels) != length(test_label_levels)) {
    stop("train_label_levels must be a list of the same length as test_label_levels")
  }
  
  
  # check that none of the same labels are in the training and test set
  same_levels_in_train_and_test_sets <- intersect(unlist(train_label_levels), unlist(test_label_levels))
  if (length(same_levels_in_train_and_test_sets) != 0){
    stop(paste0("The label(s) ", "'", same_levels_in_train_and_test_sets), "'", 
         "are in both the train_label_levels and test_label_levels")
  }
  
  
  # check that none of the the same labels are in different classes in the training set
  # (technically this could actually be ok, but the current implementation of the code
  # can't handle it)
  if (length(unlist(train_label_levels)) != length(unique(unlist(train_label_levels)))){
    stop("Some of the same levels in train_label_levels are in multiple classes")
  }
  
  
  # check that none of the the same labels are in different classes in the test set
  if (length(unlist(test_label_levels)) != length(unique(unlist(test_label_levels)))){
    stop("Some of the same levels in test_label_levels are in multiple classes")
  }
  

  # construct a ds_basic object that will do most of the work
  
  all_label_levels_to_use <- c(unlist(train_label_levels), unlist(test_label_levels))
  
  the_basic_ds <- ds_basic(binned_file_name, 
                           var_to_decode, 
                           num_cv_splits, 
                           use_count_data,
                           num_label_repeats_per_cv_split, 
                           label_levels_to_use = all_label_levels_to_use, 
                           num_resample_sites,
                           site_IDs_to_use,
                           site_IDs_to_exclude,
                           randomly_shuffled_labels_before_running,
                           create_simultaneously_recorded_populations)
    

  # create the main data structure which just consists of the ds_basic
  #  and the train and test label levels which are unique to this ds
  the_ds <- list(
    the_basic_ds = the_basic_ds,
    train_label_levels = train_label_levels,
    test_label_levels = test_label_levels)
  
  
  attr(the_ds, "class") <- "ds_generalization"
  the_ds
  
}
                      


      
get_data.ds_generalization = function(ds_generalization_obj){
        
  
  # the ds_basic does all the hard work of getting the data
  all_cv_data <- get_data(ds_generalization_obj$the_basic_ds)

  all_cv_data <- all_cv_data %>%
    mutate(train_labels = as.character(train_labels),
           test_labels = as.character(test_labels))

  train_label_levels <- ds_generalization_obj$train_label_levels
  test_label_levels <- ds_generalization_obj$test_label_levels
  
  new_train_labels <- rep("not_used", dim(all_cv_data)[1])
  new_test_labels <- rep("not_used", dim(all_cv_data)[1])
  
  # remap the train_label_levels and test_label_levels
  for (iClass in seq_along(train_label_levels)) {
    
    curr_train_levels <- train_label_levels[[iClass]]
    curr_test_levels <- test_label_levels[[iClass]]
    
    for (iLevel in seq_along(curr_train_levels)) {
      new_train_labels[all_cv_data$train_labels %in% 
                                 curr_train_levels[iLevel]] <- paste0("class_", iClass)
    }
      
    for (iLevel in seq_along(curr_test_levels)) {
      new_test_labels[all_cv_data$test_labels %in% 
                               curr_test_levels[iLevel]] <- paste0("class_", iClass)
    }
    
  }
  
  
  browser()
  
  cv_split_info <- select(all_cv_data, starts_with("CV"))
  
  cv_split_info_train <- cbind(trial_num = 1:dim(cv_split_info)[1], new_train_labels, cv_split_info) %>%
    tidyr::gather(CV, train_test, -new_train_labels, -trial_num)
  
  cv_split_info_train$train_test[cv_split_info_train$new_train_labels == "not_used"] <- "not_used"
  
  cv_split_info_train %>% 
    select(-new_train_labels) %>%
    tidyr::spread(CV, train_test) %>% View()  #, -trial_num)
  
  
  
  all_cv_data
  

}  






get_parameters.ds_generalization = function(ds_basic_obj){

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


