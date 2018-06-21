#' A Poisson Naive Bayes classifier (CL) object
#'
#' An implementation of a Poisson Naive Bayes classifier. Like all classifiers, this classifier
#' learning a model based on training data and then makes predictions on new test data.  
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#' Note: this classifier uses spike counts, so the binned data must be converted to use this classifier, 
#' for exmaple, if you are using the basic_DS data source, then use.count.data = TRUE should be set in the 
#' contructor. Also, preprocessors that convert the data into values that are not integers should not be used, 
#' for example, the zscore_FP should not be used with this classifier. 
#'
#'
#'
#' @section poisson_naive_bayes_CL constructor:
#' 
#' \describe{
#' \item{\code{poisson_naive_bayes_CL$new()}}{
#' if successful, will return a new \code{poisson_naive_bayes_CL} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{get_predictions(train.data, all.times.test.data)}}{
#' Learns a model from the train.data and then makes predictions on the
#' all.times.test.data data set. 
#' }}
#' 
#' 
#' 
#' @import R6
#' @export

poisson_naive_bayes_CL <- R6Class("poisson_naive_bayes_CL", 
   public = list(
     # no properties for this classifier
     
    # the constructor does not take any arguments
    initialize = function() {},
                         
    # methods
    # could break this up into two methods: train() and test()
    get_predictions = function(train_data, all_times_test_data) {
      ### Train the classifier
      lambdas_and_labels <- train_data %>% group_by(labels) %>% summarise_each(funs(mean))
      #class_labels <- lambdas[, 1]
      lambda_data <- as.matrix(lambdas_and_labels[, 2:ncol(lambdas_and_labels)])
      
      # If there are lambda values equal to zero this can cause problems  if some of the test data 
      # for a given lamda is not 0 (because there will be 0 probability of getting the data with lambda == 0).
      # To deal with this we are going to assume that for all lambda == 0, there is one additional training point
      # that had a value of 1, which will solve this problem. 
      num_train_examples_in_each_class <- table(train_data$labels)
      
      # if there are the same number of training examples in each class  (as there should be)
      if (sum(abs(diff(num_train_examples_in_each_class))) == 0) {
        lambda_data[lambda_data == 0] <- 1/(table(train_data$labels)[1] + 1)
      } else {
        # if there are the different numbers of training examples in different class (this really shouldn't happen)
        for (iClass in 1:length(num_train_examples_in_each_class)) {
          lambda_data[iClass, lambda_data[iClass, ] == 0] <- 1/(num_train_examples_in_each_class[iClass] + 1) 
        }
      }
      
      ### Test the classifier
      test_labels <- select(all_times_test_data, -starts_with("site"))
      test_data <- as.matrix(select(all_times_test_data, starts_with("site")))
      
      num_classes <- length(unique(test_labels$labels))
      num_sites <- dim(test_data)[2]
      num_test_points <- dim(test_data)[1]
      
      # works, but relatively slow...
      # all_pois_values <- array(NA, dim = c(num_classes, num_sites, num_test_points))
      # for (iSite in 1:num_sites) {
      #   for (iClass in 1:num_classes) {
      #     all_pois_values[iClass, iSite, ] <- dpois(test_data[, iSite], lambda_data[iClass, iSite])
      #  }
      # }
      # log_likelihoods <- apply(log(all_pois_values), MARGIN = c(1, 3), sum)
      
      # much faster way to get the log.likelihood values using linear algebra operations on matrices
      log_likelihoods <- test_data %*% t(log(lambda_data)) 
      log_likelihoods <- sweep(log_likelihoods, 2, rowSums(lambda_data))
      log_likelihoods <- t(sweep(log_likelihoods, 1, rowSums(lgamma(test_data + 1))))
      # get the predicted labels
      predicted_inds <- apply(log_likelihoods, 2, which.max)   # need to create rand.which.max() function...
      predicted_labels <- lambdas_and_labels$labels[predicted_inds]
      # create a data frame that has all the results
      results <- data.frame(time = all_times_test_data$time, actual_labels = all_times_test_data$labels, 
                            predicted_labels = predicted_labels) %>%
        mutate(correct = actual_labels == predicted_labels)
      # get the decision values
      decision_values <- data.frame(t(log_likelihoods))
      names(decision_values) <- paste0('decision_val_', lambdas_and_labels$labels)  
      # return the results
      results <- cbind(results, decision_values)
      
      return(results)
    } # end the get_predictions method
   )  # end the public properites/methods
)  # end the class














