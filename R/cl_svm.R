

# uses the e1071 package for the svm



# the constructor 
#' @export
cl_svm <- function(...){
  options <- list(...)
  the_classifier <- list(svm_options = options)
  attr(the_classifier, "class") <- "cl_svm"
  the_classifier
}



#' @export
get_predictions.cl_svm <- function(cl_svm_obj, training_set, test_set) {
  

  ### Train the classifier  ---------------------------------------------------
  #trained_svm <- svm(labels ~ ., data = training_set)
  #trained_svm <- svm(x = select(training_set, -labels), y = training_set$labels)
  
  # if arguments to the svm have been supplied, use them
  if (length(cl_svm_obj$svm_options) == 0){
    all_arguments <- list(x = select(training_set, -labels), y = training_set$labels)
  } else{
    all_arguments <- list(x = select(training_set, -labels), 
                          y = training_set$labels, 
                          unlist(cl_svm_obj$svm_options))
    names(all_arguments) <- c("x", "y", names(cl_svm_obj$svm_options))
  }

  
  trained_svm <- do.call(svm, all_arguments)
  
  
  ### Test the classifier  ---------------------------------------------------
  #predicted_labels <- predict(trained_svm, test_set, decision.values = TRUE)  
  predicted_labels <- predict(trained_svm, select(test_set, starts_with("site")), decision.values = TRUE) 
  results <- data.frame(test_time = test_set$time_bin, 
              actual_labels = test_set$labels,
              predicted_labels = predicted_labels)
          
  
  # Parse the all-pairs decision values ---------------------------------------
  all_pairs_results <- data.frame(attr(predicted_labels, "decision.values"))

  all_pairs_results <- cbind(test_point_num = 1:dim(results)[1], all_pairs_results) %>%
    tidyr::gather(class_pair, val, -test_point_num) %>%
    mutate(sign_prediction = sign(val)) %>%
    tidyr::separate(class_pair, c("pos_class", "neg_class")) #, sep = "/")
  
  pos_wins <- all_pairs_results %>%
    group_by(pos_class, test_point_num) %>%
    summarize(pos_wins = sum(sign_prediction))
  
  neg_wins <- all_pairs_results %>%
    group_by(neg_class, test_point_num) %>%
    summarize(neg_wins = sum(-1 * sign_prediction))
  
  decision_val_df <- full_join(pos_wins, neg_wins, 
                               by = c("pos_class" = "neg_class", 
                                      "test_point_num" = "test_point_num")) %>%
    tidyr::replace_na(list(pos_wins = 0, neg_wins = 0)) %>%
    mutate(tot_wins = pos_wins + neg_wins) %>%
    select(pos_class, tot_wins, test_point_num) %>%
    tidyr::spread(pos_class, tot_wins) %>%
    arrange(test_point_num) %>%
    select(-test_point_num)
  
  names(decision_val_df) <- paste0("decision_vals.", names(decision_val_df))
  
  results <- cbind(results, decision_val_df)
  
  results

}   


# Get the parameters that were used in the svm
get_parameters.cl_svm = function(cl_svm_obj){

  if (length(cl_svm_obj$svm_options) == 0){
    
    parameter_df <- data.frame(cl_svm.cl_svm = "default svm parameters")
  
    } else{

      parameter_df <- data.frame(val = unlist(cl_svm_obj$svm_options)) %>%
      mutate(key = rownames(.)) %>% 
      tidyr::spread(key, val) %>%
      mutate_all(type.convert) %>%
      mutate_if(is.factor, as.character)
      
      names(parameter_df) <- paste0("cl_svm.", names(parameter_df))
  }

  
  parameter_df 
  
}




