#' A support vector machine classifier (CL)
#'
#' This classifier uses the e1071 package to implement a support vector machine.
#'
#' @details
#'
#' A support vector machine (SVM) is a classifier that learns a function *f* that
#' minimizes the hinge loss between predictions made on the training data, while
#' also applying a penalty for more complex *f* (the penalty is based on the norm
#' of *f* in a reproducing kernel Hilbert space). The SVM has a parameter *C* that
#' controls the trade off between the empirical loss (i.e., a smaller prediction
#' error on the training set), and the complexity of the *f*. SVMs can use
#' different kernels to create nonlinear decision boundaries.
#'
#' SVMs are work on binary classification problems, so to do
#' multi-class classification, an *all-pairs* classification scheme (which is
#' the default for the e1071 package). In the all-pairs scheme,training separate
#' classifiers for all pairs of labels (i.e., if there are 100 different classes
#' then nchoosek(100, 2) = 4950 different classifiers are trained). Testing the
#' classifier in all-pairs involves having all classifiers classify the test
#' point, and then the class label is given to the class the was chosen most
#' often by the binary classifiers (in the case of a tie in the number of
#' classes that won a contest the class label is randomly chosen). The decision
#' values for all-pairs are the number of contests won by each class (for each
#' test point).
#'
#'
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the cl_svm classifier works with the magrittr pipe (%>%)
#'   operator. This argument should almost never be directly set by the user to
#'   anything other than NULL. If this is set to the default value of NULL, then
#'   the constructor will return a cl_svm object. If this is set to an ndr
#'   container, then a cl_svm object will be added to the container and the
#'   container will be returned. If this argument is set to another ndr object,
#'   then both that ndr object as well as a new cl_svm object will be added to a
#'   new container and the container will be returned.
#'   
#' @param return_decision_values A Boolean specifying whether the prediction
#'   function should return columns that have the decision values. Setting this
#'   to FALSE will save memory so can be useful when analyzing very large high
#'   temporal resolution data sets. However if this is set to FALSE< metrics
#'   won't be able to compute decoding accuracy measures that are based on the
#'   decision values; e.g., the rm_main_results object won't be able to
#'   calculate normalized rank decision values.
#'
#' @param ... All parameters that are available in the e1071 package svm()
#'   object should work with this CL object.
#'
#'
#' @return This constructor creates an NDR classifier object with the class
#'   `cl_svm`. Like all NDR classifier objects, this classifier will be used by
#'   a cross-validator to learn the relationship between neural activity and
#'   experimental conditions on a training set of data, and then it will be used
#'   to make predictions on a test set of data.
#'
#'
#' @examples
#' # using the default e1071 parameters
#' cl <- cl_svm()
#'
#' # using a linear kernel
#' cl <- cl_svm(kernel = "linear")
#' 
#' @seealso e1071
#' 
#' @family classifier





# the constructor
#' @export
cl_svm <- function(ndr_container_or_object = NULL, 
                   return_decision_values = TRUE, 
                   ...) {
  
  
  options <- list(...)
  the_classifier <- list(return_decision_values = return_decision_values, 
                         svm_options = options)
  attr(the_classifier, "class") <- "cl_svm"
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the classifier in it
  put_ndr_object_in_container(ndr_container_or_object, the_classifier)
  
  
}



# the get_predictions method
#' @inherit get_predictions
#' @keywords internal
#' @export
get_predictions.cl_svm <- function(cl_obj, training_set, test_set) {


  ### Train the classifier  ---------------------------------------------------
  
  # trained_svm <- svm(train_labels ~ ., data = training_set)
  # trained_svm <- svm(x = select(training_set, -train_labels), y = training_set$train_labels)

  # if arguments to the svm have been supplied, use them
  if (length(cl_obj$svm_options) == 0) {
    
    all_arguments <- list(x = select(training_set, -.data$train_labels), y = training_set$train_labels)

  } else {

    all_arguments <- list(
      x = select(training_set, -.data$train_labels),
      y = training_set$train_labels,
      unlist(cl_obj$svm_options))

    names(all_arguments) <- c("x", "y", names(cl_obj$svm_options))
    
  }


  trained_svm <- do.call(e1071::svm, all_arguments)


  ### Test the classifier  ---------------------------------------------------

  predicted_labels <- predict(trained_svm, select(test_set, starts_with("site")), 
                              decision.values = TRUE)
  
  results <- data.frame(
    test_time = test_set$time_bin,
    actual_labels = test_set$test_labels,
    predicted_labels = predicted_labels)


  # Parse the all-pairs decision values ---------------------------------------
  if (cl_obj$return_decision_values) {
    
    all_pairs_results <- data.frame(attr(predicted_labels, "decision.values"))
    names(all_pairs_results) <- colnames(attr(predicted_labels, "decision.values"))
  
    all_pairs_results <- cbind(test_point_num = 1:dim(results)[1], all_pairs_results) %>%
      tidyr::gather("class_pair", "val", -.data$test_point_num) %>%
      mutate(sign_prediction = sign(.data$val)) %>%
      tidyr::separate(.data$class_pair, c("pos_class", "neg_class"), sep = "/")
  
    pos_wins <- all_pairs_results %>%
      group_by(.data$pos_class, .data$test_point_num) %>%
      summarize(pos_wins = sum(.data$sign_prediction))
  
    neg_wins <- all_pairs_results %>%
      group_by(.data$neg_class, .data$test_point_num) %>%
      summarize(neg_wins = sum(-1 * .data$sign_prediction))
  
    decision_val_df <- full_join(pos_wins, neg_wins,
      by = c(
        "pos_class" = "neg_class",
        "test_point_num" = "test_point_num")) %>%
      tidyr::replace_na(list(pos_wins = 0, neg_wins = 0)) %>%
      mutate(tot_wins = pos_wins + neg_wins) %>%
      select(.data$pos_class, .data$tot_wins, .data$test_point_num) %>%
      tidyr::spread(.data$pos_class, .data$tot_wins) %>%
      arrange(.data$test_point_num) %>%
      select(-.data$test_point_num)
  
    names(decision_val_df) <- paste0("decision_vals.", names(decision_val_df))
  
    results <- cbind(results, decision_val_df)

  }
  
  
  results
  
  
}





# Get the parameters that were used in the svm
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.cl_svm <- function(ndr_obj) {

  if (length(ndr_obj$svm_options) == 0) {

    parameter_df <- data.frame(cl_svm.cl_svm = "default svm parameters")

  } else {

    parameter_df <- data.frame(val = unlist(ndr_obj$svm_options)) %>%
      tibble::rownames_to_column("key") %>%
      tidyr::spread("key", "val") %>%
      dplyr::mutate(across(where(is.factor), as.character))

    names(parameter_df) <- paste0("cl_svm.", names(parameter_df))
    
  }


  parameter_df
  
}
