

# uses the e1071 package for the svm



# the constructor 
#' @export
cl_svm <- function(){
  the_classifier <- list()
  attr(the_classifier, "class") <- "cl_svm"
  the_classifier
}



#' @export
get_predictions.cl_svm <- function(cl_svm_obj, training_set, test_set) {
  

  
  ### Train the classifier  ---------------------------------------------------
  trained_svm <- svm(labels ~ ., data = training_set)
  
  
  ### Test the classifier  ---------------------------------------------------
  predicted_labels <- predict(trained_svm, test_set, decision.values = TRUE)  
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




# Need to fill this out once I change the code to accept parameters...
get_parameters.cl_svm = function(cl_svm_obj){
  data.frame(cl_svm.cl_svm = "need to fill this out")
}




