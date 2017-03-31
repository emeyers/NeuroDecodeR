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
    get_predictions = function(train.data, all.times.test.data) {    
  

      
      ### Train the classifier
      
      prototypes <- train.data %>% group_by(labels) %>% summarise_each(funs(mean))
      
      
      
      
      ### Test the classifier
      
      
      train.test.cor <- cor(t(prototypes[, 2:dim(prototypes)[2]]), t(select(all.times.test.data, -labels, -time)))
      #train.test.cor <- cor(t(prototypes[, 2:133]), t(select(all.times.test.data, -labels, -time)))
      

      # get the predicted labels
      
      # predicted.inds <- apply(train.test.cor, 2, which.max)   # need to create rand.which.max() function...
      #predicted.inds <- apply(train.test.cor, 2, which.is.max)   # only slightly slower but breaks ties
      predicted.inds <- apply(train.test.cor, 2, rand.which.max)   # only slightly slower but breaks ties
      
      
      predicted.labels <- prototypes$labels[predicted.inds]
      
      
      # create a data frame that has all the results
      results <- data.frame(time = all.times.test.data$time, actual.labels = all.times.test.data$labels, 
                            predicted.labels = predicted.labels) %>%
        mutate(correct = actual.labels == predicted.labels)
      
      
      # get the decision values
      decision.values <- data.frame(t(train.test.cor))
      names(decision.values) <- paste0('decision.val.', prototypes$labels)  
      
      
      # return the results
      results <- cbind(results, decision.values)
      return(results)
      
    
      
      
      }   # end the get_predictions method
    
    
   )
   
   
)  # end the class














