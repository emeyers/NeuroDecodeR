#' A Poisson Naive Bayes classifier (CL)
#'
#' An implementation of a Poisson Naive Bayes classifier.
#' 
#' @param ndr_container_or_object The purpose of this argument is to make the
#'   constructor of the cl_poisson_naive_bayes classifier work with magrittr
#'   pipe (%>%) operator. This argument should almost never be directly set by
#'   the user to anything other than NULL. If this is set to the default value
#'   of NULL, then the constructor will return a cl_poisson_naive_bayes object.
#'   If this is set to an ndr container, then a cl_poisson_naive_bayes object
#'   will be added to the container and the container will be returned. If this
#'   argument is set to another ndr object, then both that ndr object as well as
#'   a new cl_poisson_naive_bayes object will be added to a new container and
#'   the container will be returned.
#'   
#' @param return_decision_values A Boolean specifying whether the prediction
#'   function should return columns that have the decision values. Setting this
#'   to FALSE will save memory so can be useful when analyzing very large high
#'   temporal resolution data sets. However if this is set to FALSE< metrics
#'   won't be able to compute decoding accuracy measures that are based on the
#'   decision values; e.g., the rm_main_results object won't be able to
#'   calculate normalized rank decision values.
#'
#' @return This constructor creates an NDR classifier object with the class
#'   `cl_poisson_naive_bayes`. Like all NDR classifier objects, this classifier
#'   will be used by a cross-validator to learn the relationship between neural
#'   activity and experimental conditions on a training set of data, and then it
#'   will be used to make predictions on a test set of data.
#'
#'
#' @details
#'
#' This classifier object implements a Poisson Naive Bayes classifier. The
#' classifier works by learning the expected number of occurrences (denoted
#' lambda) for each feature and each class by taking the average of the training
#' data over all trials (separately for each feature and each class). To
#' evaluate whether a given test point belongs to class i, the log of the
#' likelihood function is calculated using the lambda values as parameters of
#' Poisson distributions (i.e., there is a separate Poisson distribution for
#' each feature, that is based on the lambda value for that feature). The
#' overall likelihood value is calculated by multiplying the probabilities for
#' each neuron together (i.e,. Naive Bayes classifiers assume that each feature
#' is independent), or equivalently, adding the log of the probabilities for
#' each feature together. The class with the highest likelihood value is chosen
#' as the predicted label, and the decision values are the log likelihood
#' values.
#'
#' **Note:** this classifier uses spike counts, so the binned data must be
#' converted to use this classifier, for example, if you are using the basic_DS
#' data source, then use_count_data = TRUE should be set in the constructor.
#' Also, preprocessors that convert the data into values that are not integers
#' should not be used, for example, the fp_zscore should not be used with this
#' classifier.
#'
#' Like all classifiers, this classifier learning a model based on training data
#' and then makes predictions on new test data.
#'
#'
#' @examples
#' # running a basic decoding analysis using the cl_max_correlation
#'
#' data_file <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"),
#'                          package = "NeuroDecodeR")
#' ds <- ds_basic(data_file, "stimulus_ID", 18, use_count_data = TRUE)
#' fps <- list()
#'
#' cl <- cl_poisson_naive_bayes()
#' cv <- cv_standard(datasource = ds, 
#'                   classifier = cl, 
#'                   feature_preprocessors = fps,
#'                   num_resample_runs = 2)  # better to use more resample runs (default is 50)
#' \donttest{
#' DECODING_RESULTS <- run_decoding(cv)
#' }
#'
#' @family classifier



# the constructor
#' @export
cl_poisson_naive_bayes <- function(ndr_container_or_object = NULL,
                                   return_decision_values = TRUE) {
  
  the_classifier <- list(return_decision_values = return_decision_values)
  attr(the_classifier, "class") <- "cl_poisson_naive_bayes"
  
  # if ndr_container_or_object is an ndr object or ndr container, return
  #  an ndr container that has the classifier in it
  put_ndr_object_in_container(ndr_container_or_object, the_classifier)
  
}




# the get_predictions method
#' @inherit get_predictions
#' @keywords internal
#' @export
get_predictions.cl_poisson_naive_bayes <- function(cl_obj, training_set, test_set) {

  # check that the training and test data are all integers
  approx_zero <- 10^-10
  if ((abs(sum(select(training_set, starts_with("site")) %% 1)) > approx_zero) ||
      (abs(sum(select(test_set, starts_with("site")) %% 1)) > approx_zero)) {
    stop("The training and test data must only contain interger values")
  }


  # Train the classifier --------------------------------------------------
  lambdas_and_labels <- training_set %>%
    group_by(.data$train_labels) %>%
    summarise_all(mean)

  lambda_data <- as.matrix(lambdas_and_labels[, 2:ncol(lambdas_and_labels)])

  # If there are lambda values equal to zero this can cause problems if some
  # of the test data is not 0 (because there will be 0 probability of
  # getting data greater than 0 if lambda == 0). To deal with this we are
  # going to assume that for all lambda == 0, there is one additional
  # training point that had a value of 1, which will make all lambdas
  # greater than 0.
  num_train_examples_in_each_class <- table(training_set$train_labels)


  # if there are the same number of training examples in each class  (as there should be)
  if (sum(abs(diff(num_train_examples_in_each_class))) == 0) {
    
    lambda_data[lambda_data == 0] <- 1 / (table(training_set$train_labels)[1] + 1)
  
    } else {
    
      # if there are the different numbers of training examples in different class (this really shouldn't happen)
    for (iClass in seq_along(num_train_examples_in_each_class)) {
      lambda_data[iClass, lambda_data[iClass, ] == 0] <- 1 / (num_train_examples_in_each_class[iClass] + 1)
    }
      
  }


  # Test the classifier ---------------------------------------------------
  test_data <- as.matrix(select(test_set, starts_with("site")))

  # fast way to get the log-likelihood values using operations on matrices
  log_likelihoods <- test_data %*% t(log(lambda_data))
  log_likelihoods <- sweep(log_likelihoods, 2, rowSums(lambda_data))
  log_likelihoods <- t(sweep(log_likelihoods, 1, rowSums(lgamma(test_data + 1))))

  # get the predicted labels
  predicted_inds <- apply(log_likelihoods, 2, rand_which_max) # NDR helper rand_which_max()
  predicted_labels <- lambdas_and_labels$train_labels[predicted_inds]

  # create a data frame that has all the results
  results <- data.frame(
    test_time = test_set$time_bin,
    actual_labels = test_set$test_labels,
    predicted_labels = predicted_labels
  )

  
  # get the decision values
  if (cl_obj$return_decision_values) {
    
    decision_values <- data.frame(t(log_likelihoods))
    names(decision_values) <- paste0("decision_vals.", lambdas_and_labels$train_labels)
    
    results <- cbind(results, decision_values)
 }

      
  results
  
}





# since there are no parameters for the cl_poisson_naive_bayes just return a data
# frame with cl_poisson_naive_bayes.cl_poisson_naive_bayes saying no params...
#' @inherit get_parameters
#' @keywords internal
#' @export
get_parameters.cl_poisson_naive_bayes <- function(ndr_obj) {
  data.frame(cl_poisson_naive_bayes.cl_poisson_naive_bayes = "does not have settable parameters")
}
