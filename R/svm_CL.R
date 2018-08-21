# the decision values in this function are not really correct so I am not going to add it to the package yet...
library('e1071')  # svm package...

svm_CL <- R6Class("svm_CL", 
   public = list(
    # properties
                    
    # the constructor 
    initialize = function() {},
                    
    # methods
    # could break this up into two methods: train() and test()
    get_predictions = function(train_data, all_times_test_data) {
      # train the classifier
      trained_svm <- svm(labels ~ ., data = train_data)
      # test the classifier
      predicted_labels <- predict(trained_svm, all_times_test_data, decision.values = TRUE)  
      results <- data.frame(time = all_times_test_data$time, 
                  actual_labels = all_times_test_data$labels,
                  predicted_labels = predicted_labels) %>%
                  mutate(correct = actual_labels == predicted_labels)
              
      # getting an estimate of the decision values...
      all_pairs_results <- attr(predicted_labels, "decision.values")
      all_pairs_results <- data.frame(attr(predicted_labels, "decision.values"))
      aall_pairs_results_names <- names(all_pairs_results)
      
      # loop through the labels and get the average decision value for each class
      unique_labels <- as.character(unique(results$actual_labels))
      decision_values <- NULL
      
      for (iLabel in 1:length(unique_labels)) {
        eval_str <- paste0("select(all_pairs_results, starts_with(\"", unique_labels[iLabel],".\"))")                                   
        pos_results <- eval(parse(text=eval_str))
        eval_str <- paste0("select(all_pairs_results, starts_with(\".", unique_labels[iLabel],"\"))")                                   
        neg_results <- eval(parse(text=eval_str))
        decision_values <- cbind(decision_values, rowSums(pos_results) - rowSums(neg_results))
      }
      
      decision_values <- data.frame(decision_values)
      names(decision_values) <- paste0('decision_val_', unique_labels) 
      results <- cbind(results, decision_values)
      names(results) <- gsub(x = names(results), pattern = "\\.", replacement = "_")
      
      # since a voting scheme is used, the class with the highest average decision value will not necessarily
      # be the class that the classifier returns, which is bad :(. A few options are to implement my
      # one vs. all svm (as I did with the MATLAB NDT) or I could try another R svm implementation. 
      warning("The decision values calculated here have some issues. Should really implement my own one-vs-all svm multi-class scheme")         
      
      return(results)
    }   # end the get_predictions method
  )  # end the public properites/methods
)  # end the class


















