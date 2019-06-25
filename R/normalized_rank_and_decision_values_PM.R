



# get the rank results and the decision value for predicted class...
get_rank_results = function(prediction_results) {
  
  decision_vals <- select(prediction_results, starts_with("decision"))
  num_classes <- ncol(decision_vals)
  num_test_points <- nrow(decision_vals)
  
  # remove the prefix 'decision.vals' from the column names...
  the_names <- names(decision_vals)
  the_names <- unlist(strsplit(the_names, "decision_val_", fixed = TRUE))
  the_names <- the_names[the_names != ""]
  names(decision_vals) <- the_names
  decision_vals_aug <- cbind(prediction_results$actual_labels, decision_vals)
  
  
  get_rank_one_row <- function(decision_vals_aug_row) {
    actual_label <- decision_vals_aug_row[1]
    decision_vals_row <- decision_vals_aug_row[2:length(decision_vals_aug_row)]
    the_names <- names(decision_vals_row)
    the_order <- order(as.numeric(decision_vals_row), decreasing = TRUE)
    which(the_names[the_order] == actual_label)
  }
  
  normalized_rank_results <- 1 - ((apply(decision_vals_aug, 1, get_rank_one_row) - 1)/(num_classes - 1))
  
  
  
  # get the decision values for the correct label
  get_decision_vals_one_row <- function(decision_vals_aug_row) {
    decision_vals_aug_row[which(as.character(as.matrix(decision_vals_aug_row[1])) == names(decision_vals_aug_row[2:length(decision_vals_aug_row)])) + 1]
  }
  
  correct_class_decision_val <- as.numeric(apply(decision_vals_aug, 1, get_decision_vals_one_row))
  
  
  rank_and_decision_val_results <- data.frame(normalized_rank_results, correct_class_decision_val)
  
}





get_mutual_information = function(prediction_results) {
  
  
  browser()
  
  
  blah <- prediction_results %>%
    dplyr::group_by(time, actual_labels, predicted_labels) %>%
    summarize(n = n())
  
  
}







