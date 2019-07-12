


#' @export
save_and_log_results <- function(DECODING_RESULTS, save_directory_name){
  
  
  # maybe should deal with slash at the end of the save_directory_name...
  #manifest_file_name <- file.path(save_directory_name, "results_manifest.rda")
  manifest_file_name <- paste0(save_directory_name, "results_manifest.rda")
  

  # load the manifest file, or if this is the first time save results has been run, 
  #  create the manifest file and give a warning that this is the first results saved
  if (!file.exists(manifest_file_name)) {

    manifest_df <- data.frame()
    
    warning(paste("The manifest file does not exist.\n", 
            " Assuming this is the first result that is saved and creating manifest file"))
    
  } else {
    load(manifest_file_name)
  }
  
  
  # # create a name for the file that will hold the results
  # curr_time <- as.character(Sys.time())
  # curr_time <- gsub("-", "", curr_time)
  # curr_time <- gsub(":", "", curr_time)
  # curr_time <- gsub(" ", "_", curr_time)
  # rand_suffix <- paste0(round(runif(5, 0, 9)), collapse = "")
  # save_file_name <- paste(curr_time, rand_suffix, sep = "_")  

  # get the decoding parameters and add the saved file name to them
  decoding_params <- get_parameters(DECODING_RESULTS$cross_validation_paramaters)
  #decoding_params$saved_file_name <- save_file_name
  #decoding_params <- dplyr::select(decoding_params, saved_file_name, everything())
    
  
  if (!("analysis_ID" %in% decoding_params)) {
    
    decoding_params$analysis_ID <- paste0(generate_analysis_ID(), "_gensave")
    decoding_params <- dplyr::select(decoding_params, analysis_ID, everything())
  }
  

  
  # if results already exist give a warning (maybe not needed but doesn't hurt)
  if (check_results_already_exist(decoding_params, manifest_df)){
    warning("The results with the same parameters already exist. Still going ahead and saving the results.")
  }
  
  
  # add the current parameters to the manifest file
  manifest_df <- add_current_parameters_to_manifest(decoding_params, manifest_df)
  
  
  # save the results and the updated manifest file...
  save(DECODING_RESULTS, file = file.path(save_directory_name, paste0(decoding_params$analysis_ID, ".rda")))
  
  save(manifest_df, file = manifest_file_name)
  
  
}







#' @export
check_results_already_exist <- function(decoding_params, manifest_df){
  
  
  if (dim(manifest_df)[2] == 0) {
    
    # if the manifest is empty, the results have not been previosly run
    return(FALSE)  
    
  } else {
    

    
    manifest_decoding_params_added <- add_current_parameters_to_manifest(decoding_params, manifest_df)
    
    manifest_decoding_params_added <- dplyr::select(manifest_decoding_params_added, -analysis_ID)
    duplicated_results <- duplicated(manifest_decoding_params_added)
    
    
    # return last element which is a boolean indicating whether the decoding_params
    #  match any of the previously saved results 
    duplicated_results[length(duplicated_results)]
    
  }
  
}





# helper function to get the decoding_params varaibles 
# to match the manifest_df variables (maybe shouldn't be exported)
#' @export
add_current_parameters_to_manifest <- function(decoding_params, manifest_df){
  
  # if the manifest_df has no data in it, just return the decoding_params
  if (dim(manifest_df)[2] == 0) {
    return(decoding_params)
  }
  
  
  decoding_names <- names(decoding_params)
  manifest_names <- names(manifest_df)
  
  unique_decoding_names <- setdiff(decoding_names, manifest_names)
  unique_manifest_names <- setdiff(manifest_names, decoding_names)

  
  # if there are some unique variables in the manifest, add them 
  # to the decoding params with NA values
  if (length(unique_manifest_names) != 0){
    
    unique_manifest_df <- data.frame(t(rep(NA, length(unique_manifest_names))))
    names(unique_manifest_df) <- unique_manifest_names
    decoding_params <- cbind(decoding_params, unique_manifest_df)
    
    # put decoding_params results in the same order as the manifest with unique
    # decoding_params at the end
    decoding_params <- dplyr::select(decoding_params, {{manifest_names}}, everything()) 
  }

  
  # if there are some unique variables in the decoding_params, add them 
  # to the manifest with NA values
  if (length(unique_decoding_names) != 0){
    
    num_rows_manifest <- dim(manifest_df)[1]
    new_manifest_columns <- data.frame(matrix(NA, nrow = num_rows_manifest, 
                                              ncol = length(unique_decoding_names)))
    
    names(new_manifest_columns) <- unique_decoding_names
    
    manifest_df <- cbind(manifest_df, new_manifest_columns)
    
  }
  
  
  # return the manifest with the decoding parameters as the last row
  rbind(manifest_df, decoding_params)
  
}






# load DECODING_RESULTS that match the decoding_parameters
#' @export
load_decoding_results <- function(decoding_params, results_directory_name){
  
  
  manifest_file_name <- paste0(results_directory_name, "results_manifest.rda")
  #manifest_file_name <- file.path(results_directory_name, "results_manifest_.rda")
  
  
  # if the directory of results or manifest file doesn't exist, throw and error
  if (!file.exists(results_directory_name)) {
    stop(paste("The specified results_directory_name,", results_directory_name, "does not exist."))
  }
  
  if (!file.exists(manifest_file_name)) {
    stop("The manifest files doesn't exist. Check that you specified the correct results directory.")
  }
  
  
  load(manifest_file_name)
  
  if (!check_results_already_exist(decoding_params, manifest_df)){
    stop("It does not appear that results based on the parameters specified have been run yet.")
  }
  
  
  manifest_with_results_added <- add_current_parameters_to_manifest(decoding_params, manifest_df) %>%
    select(-analysis_ID)
  
  # find all rows that match the last row...
  num_manifest_rows <- dim(manifest_df)[1]
  all_decoding_results <- NULL
  c <- 1
  for (i in 1:num_manifest_rows){

    if (duplicated(manifest_with_results_added[c(i,  num_manifest_rows + 1), ])[2]){
      load(paste0(results_directory_name, manifest_df[i, ]$analysis_ID, '.rda'))
      all_decoding_results[[c]] <- DECODING_RESULTS
      c <- c + 1
    }
      
  }
  

  # if there is just one match for the results, return the DECODING_RESULTS rather
  # than a list of decoding results
  if (length(all_decoding_results) == 1){
    all_decoding_results <- all_decoding_results[[1]]
  }
  
  all_decoding_results
  
}













