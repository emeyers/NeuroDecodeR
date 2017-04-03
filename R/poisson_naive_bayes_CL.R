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
    get_predictions = function(train.data, all.times.test.data) {    
  


      ### Train the classifier
      
      lambdas.and.labels <- train.data %>% group_by(labels) %>% summarise_each(funs(mean))
      #class.labels <- lambdas[, 1]
      lambda.data <- as.matrix(lambdas.and.labels[, 2:ncol(lambdas.and.labels)])
      
     
      # If there are lambda values equal to zero this can cause problems  if some of the test data 
      # for a given lamda is not 0 (because there will be 0 probability of getting the data with lambda == 0).
      # To deal with this we are going to assume that for all lambda == 0, there is one additional training point
      # that had a value of 1, which will solve this problem. 
      
      num.train.examples.in.each.class <- table(train.data$labels)
      
      # if there are the same number of training examples in each class  (as there should be)
      if (sum(abs(diff(num.train.examples.in.each.class))) == 0) {
        lambda.data[lambda.data == 0] <- 1/(table(train.data$labels)[1] + 1)
      } else {
        # if there are the different numbers of training examples in different class (this really shouldn't happen)
        for (iClass in 1:length(num.train.examples.in.each.class)) {
          lambda.data[iClass, lambda.data[iClass, ] == 0] <- 1/(num.train.examples.in.each.class[iClass] + 1) 
        }
      }
        
      
      
      
      ### Test the classifier
      
      
      test.labels <- select(all.times.test.data, -starts_with("site"))
      test.data <- as.matrix(select(all.times.test.data, starts_with("site")))
      
      num.classes <- length(unique(test.labels$labels))
      num.sites <- dim(test.data)[2]
      num.test.points <- dim(test.data)[1]
      
      
      # works, but relatively slow...
      # all.pois.values <- array(NA, dim = c(num.classes, num.sites, num.test.points))
      # for (iSite in 1:num.sites) {
      #   for (iClass in 1:num.classes) {
      #     all.pois.values[iClass, iSite, ] <- dpois(test.data[, iSite], lambda.data[iClass, iSite])
      #  }
      # }
      # log.likelihoods <- apply(log(all.pois.values), MARGIN = c(1, 3), sum)
      
      
      # much faster way to get the log.likelihood values using linear algebra operations on matrices
      log.likelihoods <- test.data %*% t(log(lambda.data)) 
      log.likelihoods <- sweep(log.likelihoods, 2, rowSums(lambda.data))
      log.likelihoods <- t(sweep(log.likelihoods, 1, rowSums(lgamma(test.data + 1))))
      

      # get the predicted labels
      predicted.inds <- apply(log.likelihoods, 2, which.max)   # need to create rand.which.max() function...
      predicted.labels <- lambdas.and.labels$labels[predicted.inds]
      
      
      # create a data frame that has all the results
      results <- data.frame(time = all.times.test.data$time, actual.labels = all.times.test.data$labels, 
                            predicted.labels = predicted.labels) %>%
        mutate(correct = actual.labels == predicted.labels)
      
 
      # get the decision values
      decision.values <- data.frame(t(log.likelihoods))
      names(decision.values) <- paste0('decision.val.', lambdas.and.labels$labels)  
      
      
      # return the results
      results <- cbind(results, decision.values)
      return(results)
      
    
      
      
      }   # end the get_predictions method
    
    
   )  # end the public properites/methods
   
   
)  # end the class














