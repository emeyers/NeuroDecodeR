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
     preprocess_data = function(train.data, test.data){
             
       # separate the data from the labels
                  
       train.labels <- select(train.data, -starts_with('site'))
       test.labels <- select(test.data, -starts_with('site'))
       
       train.data <- select(train.data, starts_with('site'))
       test.data <- select(test.data, starts_with('site'))
       
       
       # get parameters from the training data
    
       # test code to make sure it worked
       
       # apply the parameters to the training data
       # training.means <- colMeans(train.data)
       # training.sd <- apply(train.data, 2, sd)
       # train.centered.check <- sweep(train.data, 2, training.means)
       # train.zscored.check <- sweep(train.centered.check, 2, training.sd, FUN = "/")
       
       
       # can do all the scaling of the trianing data and parameter estimation with this one line
       train.zscored <- scale(train.data, center = TRUE, scale = TRUE)
       
       
       # apply the parameters to the test data
       test.centered <- sweep(test.data, 2, attr(train.zscored, "scaled:center") )
       test.zscored <- sweep(test.centered, 2, attr(train.zscored, "scaled:scale"), FUN = "/")
    
       
       # set to 0 all InF and NAs that could occur due to the standard deviation of a site being 0
       train.zscored[is.infinite(train.zscored)] <- 0
       train.zscored[is.na(train.zscored)] <- 0
       test.zscored[is.infinite(as.matrix(test.zscored))] <- 0
       test.zscored[is.na(test.zscored)] <- 0
       
       
       # return a list with the results
       processed.data <- list(train.data = cbind(train.zscored, train.labels), 
                              test.data = cbind(test.zscored, test.labels))
          
       return(processed.data)
       
          
     }  # end preprocess_data


     
  ) # end public properties/methods
  
  
)  # end class









