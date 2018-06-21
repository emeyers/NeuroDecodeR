#' A feature preprocessor (FP) that zscore normalizes the data
#'
#' This feature prerpocessor object find the mean and standard deviation using the training data. 
#' The proprocessor then z-score transforms the training and test data using this mean and standard deviation
#' by subtracting the mean and dividing by the standard deviation. 
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section zscore_FP constructor:
#' 
#' \describe{
#' \item{\code{zscore_FP$new()}}{
#' if successful, will return a new \code{zscore_FP} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{preprocess_data}}{
#' Like all FP objects, this method finds parameters on the training set and then applies them 
#' to the training and test sets. For zscore_FP, the parameters found are the mean and the standard deviation,
#' and the transformation is a z-score transformation. 
#' }}
#' 
#' 
#' 
#' @import R6
#' @export

zscore_FP <- R6Class("zscore_FP", 
  public = list(
     # properties
                    
     # constructor
     initialize = function() {},
                      
     # methods
     preprocess_data = function(train_data, test_data){
       # separate the data from the labels
       train_labels <- select(train_data, -starts_with('site'))
       test_labels <- select(test_data, -starts_with('site'))
       train_data <- select(train_data, starts_with('site'))
       test_data <- select(test_data, starts_with('site'))

       # get parameters from the training data
    
       # test code to make sure it worked
       
       # apply the parameters to the training data
       # training_means <- colMeans(train_data)
       # training_sd <- apply(train_data, 2, sd)
       # train_centered_check <- sweep(train_data, 2, training_means)
       # train_zscored_check <- sweep(train_centered_check, 2, training_sd, FUN = "/")
       
       # can do all the scaling of the trianing data and parameter estimation with this one line
       train_zscored <- scale(train_data, center = TRUE, scale = TRUE)
       # apply the parameters to the test data
       test_centered <- sweep(test_data, 2, attr(train_zscored, "scaled:center") )
       test_zscored <- sweep(test_centered, 2, attr(train_zscored, "scaled:scale"), FUN = "/")
       # set to 0 all InF and NAs that could occur due to the standard deviation of a site being 0
       train_zscored[is.infinite(train_zscored)] <- 0
       train_zscored[is.na(train_zscored)] <- 0
       test_zscored[is.infinite(as.matrix(test_zscored))] <- 0
       test_zscored[is.na(test_zscored)] <- 0
       # return a list with the results
       processed_data <- list(train_data = cbind(train_zscored, train_labels), 
                              test_data = cbind(test_zscored, test_labels))
          
       return(processed_data)
     }  # end preprocess_data
  ) # end public properties/methods
)  # end class









