#' A datasource (DS) that allows training and testing on different but related labels
#'
#' This datasource is useful for assessing whether information is
#' invariant/abstract to particular conditions.
#'
#' Like all datasources, this datasource takes binned format data
#' and has a get_data() method that is called by a cross-validation object to
#' get training and testing splits of data that can be passed to a classifier.
#'
#'
#' @param binned_data A string that list a path to a file that has data in
#'   binned format, or a data frame of binned_data that is in binned format.
#'
#' @param labels A string specifying the name of the labels that
#'  should be decoded. This label must be one of the columns in the binned
#'  data that starts with 'label.'
#'
#' @param num_cv_splits A number specifying how many cross-validation splits
#'  should be used.
#'
#' @param train_label_levels A list that contains vectors specifying which label
#'   levels belong to which training class. Each element in the list corresponds
#'   to a class that the specified training labels will be mapped to. For
#'   example, values in the vector in the first element in the list will be
#'   mapped onto the first training class, etc.
#'
#' @param test_label_levels A list that contains vectors specifying which label
#'   levels belong to which test class. Each element in the list corresponds to
#'   a class that the specified test labels will be mapped to. For example,
#'   values in the vector in the first element in the list will be mapped onto
#'   the first test class, etc. The number of elements in this list must be the
#'   same as the number of elements in `train_label_levels`.
#'
#' @param use_count_data If the binned data is neural spike counts, then setting
#'   use_count_data = TRUE will convert the data into spike counts. This is
#'   useful for classifiers that work on spike count data, e.g., the
#'   poisson_naive_bayes_CL.
#'
#' @param num_label_repeats_per_cv_split A number specifying how many times each
#'   label level should be repeated in each cross-validation split.
#'
#' @param num_resample_sites The number of sites that should be randomly
#'   selected when constructing training and test vectors. This number needs to
#'   be less than or equal to the number of sites available that have
#'   num_cv_splits * num_label_repeats_per_cv_split repeats.
#'
#' @param site_IDs_to_use A vector of integers specifying which sites should be
#'   used.
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
#'   were recorded simultaneously, then setting this variable to 1 will cause the
#'   get_data() function to return simultaneous populations rather than
#'   pseudo-populations.
#'
#' @return This constructor creates an NDR datasource object with the class
#'   `ds_generalization`. Like all NDR datasource objects, this datasource will
#'   be used by the cross-validator to generate training and test data sets.
#'
#' @examples
#' # One can test if a neural population contains information that is position
#' # invariant by generating training data for objects presented at 'upper' and 'middle'
#' # locations, and generating test data at a 'lower' location.
#'
#' id_levels <- c("hand", "flower", "guitar", "face", "kiwi", "couch", "car")
#' train_label_levels <- NULL
#' test_label_levels <- NULL
#' for (i in seq_along(id_levels)) {
#'   train_label_levels[[i]] <- c(
#'     paste(id_levels[i], "upper", sep = "_"),
#'     paste(id_levels[i], "middle", sep = "_")
#'   )
#'   test_label_levels[[i]] <- list(paste(id_levels[i], "lower", sep = "_"))
#' }
#'
#'
#' data_file <- system.file("extdata/ZD_150bins_50sampled.Rda", package = "NeuroDecodeR")
#' ds <- ds_generalization(
#'   data_file,
#'   "combined_ID_position", 18,
#'   train_label_levels,
#'   test_label_levels
#' )
#' @family datasource




# the constructor
#' @export
ds_generalization <- function(binned_data,
                              labels,
                              num_cv_splits,
                              train_label_levels,
                              test_label_levels,
                              use_count_data = FALSE,
                              num_label_repeats_per_cv_split = 1,
                              num_resample_sites = NULL,
                              site_IDs_to_use = NULL,
                              site_IDs_to_exclude = NULL,
                              randomly_shuffled_labels = FALSE,
                              create_simultaneous_populations = 0) {


  # check the same number of classes in the training and test set
  if (length(train_label_levels) != length(test_label_levels)) {
    stop("train_label_levels must be a list of the same length as test_label_levels")
  }


  # check that none of the same labels are in class_i of the training set and
  # class_k of the test set where i != k
  for (iClass in seq_along(train_label_levels)) {
    
    train_lebels_without_class_i <- setdiff(unlist(train_label_levels), unlist(train_label_levels[[iClass]]))
    test_labels_of_class_i <- test_label_levels[[iClass]]
    levels_crossed_between_classes <- intersect(train_lebels_without_class_i, test_labels_of_class_i)

    if (length(levels_crossed_between_classes) != 0) {
      stop("The level(s) ", paste(levels_crossed_between_classes, collapse = " "), "
           are being assigned to different classes in the training and test sets which
           is not allowed since this will lead to data leakage")
    }
  }



  # check that none of the the same labels are in different classes in the training set
  # (technically this could actually be ok, but the current implementation of the code
  # can't handle it)
  if (length(unlist(train_label_levels)) != length(unique(unlist(train_label_levels)))) {
    stop("Some of the same levels in train_label_levels are in multiple classes")
  }


  # check that none of the the same labels are in different classes in the test set
  if (length(unlist(test_label_levels)) != length(unique(unlist(test_label_levels)))) {
    stop("Some of the same levels in test_label_levels are in multiple classes")
  }


  # construct a ds_basic object that will do most of the work

  # using unique() here because for generalization analysis it is ok for some of the same labels
  # to be listed in the training and test label levels
  all_label_levels_to_use <- unique(c(unlist(train_label_levels), unlist(test_label_levels)))

  the_basic_ds <- ds_basic(binned_data,
    labels,
    num_cv_splits,
    use_count_data,
    num_label_repeats_per_cv_split,
    label_levels = all_label_levels_to_use,
    num_resample_sites,
    site_IDs_to_use,
    site_IDs_to_exclude,
    randomly_shuffled_labels,
    create_simultaneous_populations)


  # create the main data structure which just consists of the ds_basic
  #  and the train and test label levels which are unique to this ds
  the_ds <- list(
    the_basic_ds = the_basic_ds,
    train_label_levels = train_label_levels,
    test_label_levels = test_label_levels)


  attr(the_ds, "class") <- "ds_generalization"
  the_ds

}





#' @inherit get_data
#' @keywords internal
#' @export
get_data.ds_generalization <- function(ds_obj) {


  # the ds_basic does all the hard work of getting the data
  all_cv_data <- get_data(ds_obj$the_basic_ds)

  all_cv_data <- all_cv_data %>%
    mutate(
      train_labels = as.character(.data$train_labels),
      test_labels = as.character(.data$test_labels))

  train_label_levels <- ds_obj$train_label_levels
  test_label_levels <- ds_obj$test_label_levels

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



  # reformat the CV columns so that if a training or test point is not used on a give
  #  cv split it will be set to "not_used" so that the classifier will ignore it

  cv_split_info <- select(all_cv_data, starts_with("CV"))

  cv_split_info_train_test <- cbind(
    trial_num = 1:dim(cv_split_info)[1],
    new_train_labels, new_test_labels, cv_split_info) %>%
    tidyr::gather("CV", "train_test", -.data$new_train_labels, -.data$new_test_labels, -.data$trial_num)


  train_cv_inds <- which(cv_split_info_train_test$train_test == "train")
  not_used_train_label_inds <- which(cv_split_info_train_test$new_train_labels == "not_used")
  intersect_train_not_used_cv_inds <- intersect(train_cv_inds, not_used_train_label_inds)
  cv_split_info_train_test$train_test[intersect_train_not_used_cv_inds] <- "not_used"

  test_cv_inds <- which(cv_split_info_train_test$train_test == "test")
  not_used_test_label_inds <- which(cv_split_info_train_test$new_test_labels == "not_used")
  intersect_test_not_used_cv_inds <- intersect(test_cv_inds, not_used_test_label_inds)
  cv_split_info_train_test$train_test[intersect_test_not_used_cv_inds] <- "not_used"


  cv_split_info_remapped <- cv_split_info_train_test %>%
    select(-new_train_labels, -new_test_labels) %>%
    tidyr::spread("CV", "train_test")

  remapped_all_cv_data <- all_cv_data %>%
    mutate(
      train_labels = new_train_labels,
      test_labels = new_test_labels) %>%
    select(-starts_with("CV")) %>%
    cbind(cv_split_info_remapped) %>%
    select(-.data$trial_num)

  
  remapped_all_cv_data
  
}






#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.ds_generalization <- function(ndr_obj) {

  # get most of the parameters from the ds_basic
  parameter_df <- get_parameters(ndr_obj$the_basic_ds) %>%
    select(-.data$ds_basic.label_levels)

  # rename them to ds_generalization
  the_names <- names(parameter_df)
  names(parameter_df) <- sub("ds_basic", "ds_generalization", the_names)

  # add the remapping train and test label levels
  parameter_df$train_label_levels <- list(ndr_obj$train_label_levels)
  parameter_df$test_label_levels <- list(ndr_obj$test_label_levels)

  parameter_df

}
