#' An support vector machine (CL) object
#'
#' This classifier uses the e1071 package to implement a support vector machine. 
#' 
#' @details 
#' The multi-class classification is based on the number of "wins" each class
#' has using all-pairs classification scheme (the default for the e1071
#' package). The predicted label is the class with the most wins. The decision
#' values are the number of all-pairs "wins" each each class has. Thus, the
#' maximum number for a decision value is thus number of classes - 1, which is
#' the maximum number of "wins" a given class could have vs. all other classes.
#'
#'
#' @param ... all parameters that are available in the e1071 package should work
#'  with this CL object 
#' 
#' 
#' 
#' @examples
#' # using the default e1071 parameters
#' cl <- cl_svm()
#' 
#' # using a linear kernel
#' cl <- cl_svm(kernel = "linear")
#'  
#' @seealso e1071
#' @family classifier





# the constructor 
#' @export
cl_svm <- function(...){
  options <- list(...)
  the_classifier <- list(svm_options = options)
  attr(the_classifier, "class") <- "cl_svm"
  the_classifier
}



#' @export
get_predictions.cl_svm <- function(cl_obj, training_set, test_set) {
  

  ### Train the classifier  ---------------------------------------------------
  #trained_svm <- svm(train_labels ~ ., data = training_set)
  #trained_svm <- svm(x = select(training_set, -train_labels), y = training_set$train_labels)
  
  # if arguments to the svm have been supplied, use them
  if (length(cl_obj$svm_options) == 0){
    all_arguments <- list(x = select(training_set, -.data$train_labels), y = training_set$train_labels)
  } else{
    all_arguments <- list(x = select(training_set, -.data$train_labels), 
                          y = training_set$train_labels, 
                          unlist(cl_obj$svm_options))
    names(all_arguments) <- c("x", "y", names(cl_obj$svm_options))
  }

  
  trained_svm <- do.call(svm, all_arguments)
  
  
  ### Test the classifier  ---------------------------------------------------
  #predicted_labels <- predict(trained_svm, test_set, decision.values = TRUE)  
  predicted_labels <- predict(trained_svm, select(test_set, starts_with("site")), decision.values = TRUE) 
  results <- data.frame(test_time = test_set$time_bin, 
              actual_labels = test_set$test_labels,
              predicted_labels = predicted_labels)
          
  
  # Parse the all-pairs decision values ---------------------------------------
  all_pairs_results <- data.frame(attr(predicted_labels, "decision.values"))
  names(all_pairs_results) <- colnames(attr(predicted_labels, "decision.values"))

  all_pairs_results <- cbind(test_point_num = 1:dim(results)[1], all_pairs_results) %>%
    tidyr::gather("class_pair", "val", -.data$test_point_num) %>%
    mutate(sign_prediction = sign(.data$val)) %>%
    tidyr::separate(.data$class_pair, c("pos_class", "neg_class"), sep = "/")
  
  pos_wins <- all_pairs_results %>%
    group_by(.data$pos_class, .data$test_point_num) %>%
    summarize(pos_wins = sum(.data$sign_prediction))
  
  neg_wins <- all_pairs_results %>%
    group_by(.data$neg_class, .data$test_point_num) %>%
    summarize(neg_wins = sum(-1 * .data$sign_prediction))
  
  decision_val_df <- full_join(pos_wins, neg_wins, 
                               by = c("pos_class" = "neg_class", 
                                      "test_point_num" = "test_point_num")) %>%
    tidyr::replace_na(list(pos_wins = 0, neg_wins = 0)) %>%
    mutate(tot_wins = pos_wins + neg_wins) %>%
    select(.data$pos_class, .data$tot_wins, .data$test_point_num) %>%
    tidyr::spread(.data$pos_class, .data$tot_wins) %>%
    arrange(.data$test_point_num) %>%
    select(-.data$test_point_num)
  
  names(decision_val_df) <- paste0("decision_vals.", names(decision_val_df))
  
  results <- cbind(results, decision_val_df)

  results

}   





# Get the parameters that were used in the svm
#' @export
get_parameters.cl_svm = function(ndtr_obj){

  if (length(ndtr_obj$svm_options) == 0){
    
    parameter_df <- data.frame(cl_svm.cl_svm = "default svm parameters")
  
    } else{

      parameter_df <- data.frame(val = unlist(ndtr_obj$svm_options)) %>%
        tibble::rownames_to_column("key") %>% 
        tidyr::spread("key", "val") %>%
        dplyr::mutate(across(where(is.factor), as.character))
        
      names(parameter_df) <- paste0("cl_svm.", names(parameter_df))
  }

  
  parameter_df 
  
}




