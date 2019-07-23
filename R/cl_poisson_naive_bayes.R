#' A Poisson Naive Bayes classifier (CL) object
#'
#' An implementation of a Poisson Naive Bayes classifier. Like all classifiers,
#' this classifier learning a model based on training data and then makes
#' predictions on new test data.
#' 
#' 
#' @details 
#' Note: this classifier uses spike counts, so the binned data must be converted
#' to use this classifier, for exmaple, if you are using the basic_DS data
#' source, then use_count_data = TRUE should be set in the contructor. Also,
#' preprocessors that convert the data into values that are not integers should
#' not be used, for example, the fp_zscore should not be used with this
#' classifier.
#'
#' @examples 
#'  ds <- ds_basic(basedir_file_name, 'stimulus_ID', 18, use_count_data = TRUE)
#'  fps <- list()
#'  cl <- cl_poisson_naive_bayes()
#'  rms <- list(rm_main_results())
#'  cv <- cv_standard(ds, cl, fps, 3, rms) 
#'  
#'
#' @family classifier




# the constructor 
#' @export
cl_poisson_naive_bayes <- function(){
  the_classifier <- list()
  attr(the_classifier, "class") <- "cl_poisson_naive_bayes"
  the_classifier
}




get_predictions.cl_poisson_naive_bayes <- function(cl_pnb_obj, training_set, test_set) {
  
  
  # check that the training and test data are all integers
  approx_zero <- 10^-10
  if ((abs(sum(select(training_set, starts_with("site")) %% 1)) > approx_zero) ||
      (abs(sum(select(test_set, starts_with("site")) %% 1)) > approx_zero)){
    stop("The training and test data must only contain interger values")
  }
  
  
  # Train the classifier --------------------------------------------------
  lambdas_and_labels <- training_set %>% 
    group_by(train_labels) %>% 
    #summarise_all(funs(mean))
    summarise_all(mean)

  # class_labels <- lambdas[, 1]
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
    lambda_data[lambda_data == 0] <- 1/(table(training_set$train_labels)[1] + 1)
  } else {
    # if there are the different numbers of training examples in different class (this really shouldn't happen)
    for (iClass in 1:length(num_train_examples_in_each_class)) {
      lambda_data[iClass, lambda_data[iClass, ] == 0] <- 1/(num_train_examples_in_each_class[iClass] + 1) 
    }
  }
  
  
  
  # Test the classifier ---------------------------------------------------
  test_labels <- select(test_set, -starts_with("site"))
  test_data <- as.matrix(select(test_set, starts_with("site")))
  
  num_classes <- length(unique(test_labels$test_labels))
  num_sites <- dim(test_data)[2]
  num_test_points <- dim(test_data)[1]
  
  # fast way to get the log-likelihood values using operations on matrices
  log_likelihoods <- test_data %*% t(log(lambda_data)) 
  log_likelihoods <- sweep(log_likelihoods, 2, rowSums(lambda_data))
  log_likelihoods <- t(sweep(log_likelihoods, 1, rowSums(lgamma(test_data + 1))))
  
  # get the predicted labels
  predicted_inds <- apply(log_likelihoods, 2, rand_which_max)   # NDTr helper rand_which_max() 
  predicted_labels <- lambdas_and_labels$train_labels[predicted_inds]
  
  # create a data frame that has all the results
  results <- data.frame(test_time = test_set$time_bin, 
                        actual_labels = test_set$test_labels, 
                        predicted_labels = predicted_labels)
  
  # get the decision values
  decision_values <- data.frame(t(log_likelihoods))
  names(decision_values) <- paste0('decision_vals.', lambdas_and_labels$train_labels)  
  
  # return the results
  results <- cbind(results, decision_values)
  
  results

} 



# since there are no parameters for the cl_max_correlation just return a data frame with
# cl_poisson_naive_bayes.cl_poisson_naive_bayes and a value of "no parameters"
get_parameters.cl_poisson_naive_bayes = function(cl_pnb_obj){
  data.frame(cl_poisson_naive_bayes.cl_poisson_naive_bayes = "does not have settable parameters")
}










