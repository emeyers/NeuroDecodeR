#' The standard cross-validator (CV)  object
#'
#' A cross-validator object takes a datasource, feature preprocessors and a classifier
#' and runs multiple cross-validation cycles by getting new training and test data splits, 
#' running the preprocessor to do preprocessing of the data, trains and tests the classifier, and 
#' the creates metric to evaluation the classification performance on the test set.  
#' This object uses \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{R6 package} 
#'
#'
#' @section standard_CV:
#' 
#' \describe{
#' \item{\code{standard_CV$new(data.source, classifier, feature.preprocessors)  )}}{
#' If successful, will return a new \code{basic_DS} object.
#' }}
#' 
#' @section Methods
#' \describe{
#' \item{\code{run_decoding}}{
#' The main method that runs the cross-validation analysis. 
#' }}
#' 
#' 
#' 
#' @import R6
#' @export

standard_CV <- R6Class("standard_CV", 
                    
    public = list(
                      
      # properties
      data.source = NA,
      classifier = NA,
      feature.preprocessors = NA,
      num.resample.runs = 50,
      
                      
      # constructor
      initialize = function(data.source, classifier, feature.preprocessors) {
                        
        self$data.source <- data.source
        self$classifier <- classifier
        self$feature.preprocessors <- feature.preprocessors
                      
        },
                      
                      
                      
      # methods
      run_decoding = function(){


        data.source <- self$data.source
        classifier = self$classifier
        feature.preprocessors = self$feature.preprocessors
        num.resample.runs = self$num.resample.runs
        

        
        DECODING_RESULTS <- NULL
        
        
        # Add a loop over resample runs...

        
        
        # get the data from the current cross-validation run        
        cv.data <- data.source$get_data()

        
        unique.times <- unique(cv.data$time)
        num.time.bins <- length(unique.times)
        
        all.cv.train.test.inds <- select(cv.data, starts_with("CV"))
        num.CV <- ncol(all.cv.train.test.inds)
        
        
        # add names for the different dimensions of the results
        time.names <- grep("^time", names(data.source$binned.data), value = TRUE)
        dim.names <- list(1:num.CV, time.names, time.names)
        
        zero.one.loss.results <- array(NA, c(num.CV, num.time.bins, num.time.bins), dimnames = dim.names)
        decision.value.results <- array(NA, c(num.CV, num.time.bins, num.time.bins), dimnames = dim.names)
        rank.results <- array(NA, c(num.CV, num.time.bins, num.time.bins), dimnames = dim.names)
        
        
        for (iCV in 1:num.CV) {
          
          tic()
          print(iCV) 
          
          for (iTrain in 1:num.time.bins) {
            
            train.data <- filter(cv.data, time == unique.times[iTrain], all.cv.train.test.inds[iCV] == "train") %>% select(starts_with("site"), labels)

            test.data <- filter(cv.data, all.cv.train.test.inds[iCV] == "test") %>% select(starts_with("site"), labels, time)
            
            
            
            # if feature-processors have been specified, do feature processing...
            if (length(feature.preprocessors) > 1) {
            
              for (iFP in 1:length(feature.preprocessors)) {
                
                # get the preprocessed data...
                processed.data <- fps[[iFP]]$preprocess_data(train.data, test.data)
                
                # update the training and test data with this preprocessed data...
                train.data <- processed.data$train.data
                test.data <- processed.data$test.data
                
              }
            
            }  # end the if statement for doing preprocessing
            
            
            
            results <- classifier$get_predictions(train.data, test.data)
            
            
            # add more measures of decoding accuracy (rank results, etc)
            rank.and.decision.val.results <- private$get.rank.results(results)
            
            results <- cbind(results, rank.and.decision.val.results)

            
            # get the results averaged over all classes for each time period
            mean.decoding.results <- results %>% group_by(time) %>% 
              summarize(mean.accuracy = mean(correct), 
                        mean.rank = mean(normalized.rank.results),
                        mean.decision.vals = mean(correct.class.decision.val)
                        )
            
            
            zero.one.loss.results[iCV, iTrain, ] <- mean.decoding.results$mean.accuracy
            decision.value.results[iCV, iTrain, ] <- mean.decoding.results$mean.decision.vals 
            rank.results[iCV, iTrain, ] <- mean.decoding.results$mean.rank
            
            
          }   # end the for loop over time bins
          
          
          toc()
        }  # end the for loop over CV splits
        
        
        # combine all the results in one list to be returned
        
        DECODING_RESULTS$zero.one.loss.results <- zero.one.loss.results
        DECODING_RESULTS$decision.value.results <- decision.value.results
        DECODING_RESULTS$rank.results <- rank.results
        
        return(DECODING_RESULTS)
        

  }  # end the run_decoding method


  ),  # end the public methods

  
  
  # private methods
  private = list(
    
    
    # get the rank results and the decision value for predicted class...
    get.rank.results = function(results) {
      
      
      decision.vals <- select(results, starts_with("decision"))
      
      num.classes <- ncol(decision.vals)
      num.test.points <- nrow(decision.vals)
      
      # remove the prefix 'decision.vals' from the column names...
      the.names <- names(decision.vals)
      the.names <- unlist(strsplit(the.names, "decision.val.", fixed = TRUE))  
      the.names <- the.names[the.names != ""]
      names(decision.vals) <- the.names
      
      

      decision.vals.aug <- cbind(results$actual.labels, decision.vals)
      #i <- 1; decision.vals.aug.row <- decision.vals.aug [i, ]
    

      ####  Bad code - doesn't work with negative numbers because apply converts values to strings before sorting
      ### (and then the negative sign becomes a dash so it sorts the values in the wrong order)
      ##  # get the normalized rank results (the lines below are not working correcly with negative values...)      
      ##  get.rank.one.row <- function(decision.vals.aug.row) {
      ##    which(names(sort(decision.vals.aug.row[2:length(decision.vals.aug.row)], decreasing = TRUE)) == as.character(as.matrix(decision.vals.aug.row[1]))) 
      ##  }
       
  
      # This code is written less compactly but it works correctly with negative decision values
      get.rank.one.row <- function(decision.vals.aug.row) {
        actual.label <- decision.vals.aug.row[1]
        decision.vals.row <- decision.vals.aug.row[2:length(decision.vals.aug.row)]
        the.names <- names(decision.vals.row)  
        the.order <- order(as.numeric(decision.vals.row), decreasing = TRUE)
        which(the.names[the.order] == actual.label) 
      }
      normalized.rank.results <- 1 - ((apply(decision.vals.aug, 1, get.rank.one.row) - 1)/(num.classes - 1))

      
      # get the decision values for the correct label      
      get.decision.vals.one.row <- function(decision.vals.aug.row) {
        decision.vals.aug.row[which(as.character(as.matrix(decision.vals.aug.row[1])) == names(decision.vals.aug.row[2:length(decision.vals.aug.row)])) + 1]
      }
      
      correct.class.decision.val <- as.numeric(apply(decision.vals.aug, 1, get.decision.vals.one.row))
      

      
    ##  # much slower code (though potentially easier to read)
    ##  normalized.rank.results <- rep(NA, num.test.points)
    ##  correct.class.decision.val <- rep(NA, num.test.points)
    ##  for (iTestPoint in 1:num.test.points){
    ##    curr.sorted.decision.vals <- sort(decision.vals[iTestPoint, ], decreasing = TRUE) 
    ##    curr.rank.result <- which(names(curr.sorted.decision.vals) == results$actual.labels[iTestPoint])
    ##    normalized.rank.results[iTestPoint] <- 1 - ((curr.rank.result - 1)/(num.classes - 1))
    ##    correct.class.decision.val[iTestPoint] <- decision.vals[iTestPoint, which(results$actual.labels[iTestPoint] == the.names)]
    ##  }

      
      rank.and.decision.val.results <- data.frame(normalized.rank.results, correct.class.decision.val)

      
     }
  
  
  )  # end private properties/methods
  
  

) # end the class 














