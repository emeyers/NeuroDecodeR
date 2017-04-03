

# the decision values in this function are not really correct so I am not going to add it to the package yet...


library('e1071')  # svm package...



svm_CL <- R6Class("svm_CL", 
                  
   public = list(
                    
      
    # properties
                    
                    
    # the constructor 
    initialize = function() {},
                    
                    
                    
    # methods
                    
                
    # could break this up into two methods: train() and test()
    get_predictions = function(train.data, all.times.test.data) {
              
                
      # train the classifier
      trained.svm <- svm(labels ~ ., data = train.data)
        
        
      # test the classifier
      predicted.labels <- predict(trained.svm, all.times.test.data, decision.values = TRUE)  
                      
      
      results <- data.frame(time = all.times.test.data$time, 
                  actual.labels = all.times.test.data$labels,
                  predicted.labels = predicted.labels) %>%
                  mutate(correct = actual.labels == predicted.labels)
                 
      
      
      
      # getting an estimate of the decision values...
      

      all.pairs.results <- attr(predicted.labels, "decision.values")
      
      
      
      all.pairs.results <- data.frame(attr(predicted.labels, "decision.values"))
      all.pairs.results.names <- names(all.pairs.results)
      
      
      # loop through the labels and get the average decision value for each class
      unique.labels <- as.character(unique(results$actual.labels))
      
      decision.values <- NULL
      for (iLabel in 1:length(unique.labels)) {
        
  
        eval.str <- paste0("select(all.pairs.results, starts_with(\"", unique.labels[iLabel],".\"))")                                   
        pos.results <- eval(parse(text=eval.str))
                
        eval.str <- paste0("select(all.pairs.results, starts_with(\".", unique.labels[iLabel],"\"))")                                   
        neg.results <- eval(parse(text=eval.str))
        
        decision.values <- cbind(decision.values, rowSums(pos.results) - rowSums(neg.results))
        
      }
      
  
      decision.values <- data.frame(decision.values)
      names(decision.values) <- paste0('decision.val.', unique.labels) 
      
      results <- cbind(results, decision.values)
             
      
      # since a voting scheme is used, the class with the highest average decision value will not necessarily
      # be the class that the classifier returns, which is bad :(. A few options are to implement my
      # one vs. all svm (as I did with the MATLAB NDT) or I could try another R svm implementation. 
      
      warning("The decision values calculated here have some issues. Should really implement my own one-vs-all svm multi-class scheme")         
      
            
      return(results)
                      
                 
     
    }   # end the get_predictions method
                    
                    
                  
  )  # end the public properites/methods
                  
                              
)  # end the class


















