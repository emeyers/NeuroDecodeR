#' A maximum correlation coefficient classifier (CL) object
#'
#' An implementation of a maximum correlation coefficeint classifier. Like all classifiers, this classifier
#' learning a model based on training data and then makes predictions on new test data.  
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section max_correlation_CL constructor:
#' 
#' \describe{
#' \item{\code{max_correlation_CL$new()}}{
#' if successful, will return a new \code{max_correlation_CL} object.
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

max_correlation_CL <- R6Class("max_correlation_CL", 
  public = list(
    # no properties for this classifier
    
    # the constructor does not take any arguments
    initialize = function() {},
                         
    # methods
    # could break this up into two methods: train() and test()
    get_predictions = function(train_data, all_times_test_data) {    
      ### Train the classifier
      prototypes <- train_data %>% group_by(labels) %>% summarise_each(funs(mean))
      ### Test the classifier
      train_test_cor <- cor(t(prototypes[, 2:dim(prototypes)[2]]), t(select(all_times_test_data, -labels, -time)))
      #train_test_cor <- cor(t(prototypes[, 2:133]), t(select(all_times_test_data, -labels, -time)))

      # get the predicted labels
      # predicted_inds <- apply(train_test_cor, 2, which.max)   # need to create rand.which.max() function...
      #predicted_inds <- apply(train_test_cor, 2, which.is.max)   # only slightly slower but breaks ties
      predicted_inds <- apply(train_test_cor, 2, rand_which_max)   # only slightly slower but breaks ties
      predicted_labels <- prototypes$labels[predicted_inds]
      # create a data frame that has all the results
      results <- data.frame(time = all_times_test_data$time, actual_labels = all_times_test_data$labels, 
                            predicted_labels = predicted_labels) %>%
        mutate(correct = actual_labels == predicted_labels)
      # get the decision values
      decision_values <- data.frame(t(train_test_cor))
      names(decision_values) <- paste0('decision_val_', prototypes$labels)  
      # return the results
      results <- cbind(results, decision_values)
      
      return(results)
    } # end the get_predictions method
  )
)  # end the class














