#'  Saves the DECODING_RESULTS and logs the parameters used in the analysis
#'
#' This function takes results returned by the cross-validator's `run_decoding()`
#' method and uses the cross-validator's `get_properties()` method to save a log
#' of the results that be used to reload the results.
#'
#' @param DECODING_RESULTS A list of results returned by the cross-validator's
#'   run_decoding method.
#'
#' @param save_directory_name A string specifying the directory name where the
#'   decoding results should be saved.
#'
#' @param result_name A string that gives a human readable name for the results
#'   that are to be saved. This name can be used to load the results later. The
#'   default value is "No result name set".
#'   
#' @return Does not return a value but instead creates a directory that stores
#'   an .rda file with the decoding results and either creates or updates a
#'   manifest files that has information about the decoding results.
#'
#'
#' @export
log_save_results <- function(DECODING_RESULTS, 
                             save_directory_name, 
                             result_name = "No result name set") {


  # if the directory name does not end with a slash, add a slash to the directory name
  save_directory_name <- add_last_character_to_directory_name(save_directory_name)

  if (!dir.exists(save_directory_name)) {
    dir.create(save_directory_name, recursive = TRUE)
    message("The directory to save the results does not exist so it is being created now.")
  }
  
  
  manifest_file_name <- paste0(save_directory_name, "results_manifest.rda")


  # load the manifest file, or if this is the first time save results has been run,
  #  create the manifest file and give a warning that this is the first results saved
  if (!file.exists(manifest_file_name)) {
    
    manifest_df <- data.frame()

    warning(paste(
      "The manifest file does not exist.\n",
      " Assuming this is the first result that is saved and creating manifest file"))
    
  } else {
    load(manifest_file_name)
  }


  # get the decoding parameters and add an analysis_ID if it does not exist
  decoding_params <- get_parameters(DECODING_RESULTS$cross_validation_paramaters)
  if (!("analysis_ID" %in% names(decoding_params))) {
    decoding_params$analysis_ID <- paste0(generate_analysis_ID(), "_gensave")
  }


  # add the result_name (human readible name of the results) and put the results in order
  decoding_params <- decoding_params %>%
    dplyr::mutate(result_name = result_name) %>%
    select(.data$analysis_ID, .data$result_name, everything())


  # if results already exist give a warning (maybe not needed but doesn't hurt)
  if (log_check_results_already_exist(decoding_params, manifest_df)) {
    warning("The results with the same parameters already exist. Still going ahead and saving the results.")
  }


  # add the current parameters to the manifest file
  manifest_df <- add_current_parameters_to_manifest(decoding_params, manifest_df)

  # save the results and the updated manifest file...
  save(DECODING_RESULTS, file = file.path(save_directory_name, paste0(decoding_params$analysis_ID, ".rda")))

  save(manifest_df, file = manifest_file_name)
  
}






#'  A function that checks if a decoding analysis has already been run
#'
#' @param decoding_params A data frame of decoding parameters that can
#'  be created by calling the cross-validator's `get_parameters()` method.
#'
#' @param manifest_df A manifest data frame that has the list of parameters for
#'   which decoding analyses have already been run.
#'  
#' @return returns a Boolean indicating if results with a given set of 
#' parameters already exist in the manifest data frame.
#'
#'
#' @export
log_check_results_already_exist <- function(decoding_params, manifest_df) {
  
  if (dim(manifest_df)[2] == 0) {

    # if the manifest is empty, the results have not been previously run
    return(FALSE)
    
  } else {
    
    manifest_decoding_params_added <- add_current_parameters_to_manifest(decoding_params, manifest_df)

    manifest_decoding_params_added <- dplyr::select(
      manifest_decoding_params_added,
      -starts_with("analysis"), -.data$result_name)

    duplicated_results <- duplicated(manifest_decoding_params_added)

    # return last element which is a boolean indicating whether the decoding_params
    #  match any of the previously saved results
    duplicated_results[length(duplicated_results)]
    
  }
  
}






# helper function to get the decoding_params varaibles
# to match the manifest_df variables (not exporting this for the moment)
add_current_parameters_to_manifest <- function(decoding_params, manifest_df) {

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
  if (length(unique_manifest_names) != 0) {
    
    unique_manifest_df <- data.frame(t(rep(NA, length(unique_manifest_names))))
    names(unique_manifest_df) <- unique_manifest_names
    decoding_params <- cbind(decoding_params, unique_manifest_df)

    # put decoding_params results in the same order as the manifest with unique
    # decoding_params at the end
    decoding_params <- dplyr::select(decoding_params, {{ manifest_names }}, everything())
    
  }


  # if there are some unique variables in the decoding_params, add them
  # to the manifest with NA values
  if (length(unique_decoding_names) != 0) {
    
    num_rows_manifest <- dim(manifest_df)[1]
    new_manifest_columns <- data.frame(matrix(NA,
      nrow = num_rows_manifest,
      ncol = length(unique_decoding_names)))

    names(new_manifest_columns) <- unique_decoding_names

    manifest_df <- cbind(manifest_df, new_manifest_columns)
    
  }


  # return the manifest with the decoding parameters as the last row
  rbind(manifest_df, decoding_params)
  
}







#' A function that loads DECODING_RESULTS based on decoding_parameters
#'
#' @param decoding_params A data frame of decoding parameters that can
#'  be created by calling the cross-validator's `get_parameters()` method.
#'
#' @param results_dir_name A string containing the path to a directory
#'   that contains all the decoding results.
#'   
#' @return A list that has all the DECODING_RESULTS that match the parameters
#'   that were specified. If only a single result matches the parameters
#'   specified, then this DECODING_RESULTS is returned rather than a list of
#'   DECODING_RESULTS.
#'
#'
#' @export
log_load_results_from_params <- function(decoding_params, results_dir_name) {


  # if the directory name does not end with a slash, add a slash to the directory name
  results_dir_name <- add_last_character_to_directory_name(results_dir_name)

  manifest_file_name <- paste0(results_dir_name, "results_manifest.rda")


  # if the directory of results or manifest file doesn't exist, throw and error
  if (!file.exists(file.path(dirname(results_dir_name), basename(results_dir_name)))) {
    stop(paste("The specified results_dir_name,", results_dir_name, "does not exist."))
  }

  if (!file.exists(manifest_file_name)) {
    
    stop(paste(
      "The manifest files doesn't exist in the specified path", manifest_file_name, ".",
      "Check that you specified the correct results directory."))

    # added this line to get rid of R CMD check note: no visible binding for global variable 'manifest_df'
    manifest_df <- NULL
    
  }


  load(manifest_file_name)

  if (!log_check_results_already_exist(decoding_params, manifest_df)) {
    
    stop("It does not appear that results based on the parameters specified have been run yet.")

    # added this line to get rid of R CMD check note: no visible binding for global variable 'DECODING_RESULTS'
    DECODING_RESULTS <- NULL
  }


  manifest_with_results_added <- add_current_parameters_to_manifest(decoding_params, manifest_df) %>%
    select(-.data$analysis_ID, -.data$result_name)

  # find all rows that match the last row...
  num_manifest_rows <- dim(manifest_df)[1]
  all_decoding_results <- NULL
  c <- 1
  for (i in 1:num_manifest_rows) {
    
    if (duplicated(manifest_with_results_added[c(i, num_manifest_rows + 1), ])[2]) {
      load(paste0(results_dir_name, manifest_df[i, ]$analysis_ID, ".rda"))
      all_decoding_results[[c]] <- DECODING_RESULTS
      c <- c + 1
    }
    
  }


  # if there is just one match for the results, return the DECODING_RESULTS rather
  # than a list of decoding results
  if (length(all_decoding_results) == 1) {
    all_decoding_results <- all_decoding_results[[1]]
  }

  all_decoding_results
}







#' A function that loads DECODING_RESULTS based on the result_name
#'
#' @param result_name A string a specifying the result that should be loaded
#'   based on the name given. This result_name can be a regular expression in
#'   which all result_name values that match the regular expression will be
#'   returned as a list.
#'
#' @param results_dir_name A string containing the path to a directory
#'   that contains all the decoding results.
#'   
#' @return A list that has all the DECODING_RESULTS that match the `result_name`
#'   argument value in the manifest file's `result_name` column. If
#'   `result_name` argument matches only one result, then this DECODING_RESULTS
#'   is returned rather than a list of DECODING_RESULTS.
#'
#'
#' @export
log_load_results_from_result_name <- function(result_name, results_dir_name) {
  
  results_dir_name <- add_last_character_to_directory_name(results_dir_name)
  manifest_file_name <- paste0(results_dir_name, "results_manifest.rda")


  # if the directory of results or manifest file doesn't exist, throw and error
  if (!file.exists(file.path(dirname(results_dir_name), basename(results_dir_name)))) {
    stop(paste("The specified results_dir_name,", results_dir_name, "does not exist."))
  }

  if (!file.exists(manifest_file_name)) {
    
    stop(paste(
      "The manifest files doesn't exist in the specified path", manifest_file_name, ".",
      "Check that you specified the correct results directory."))

    # added this line to get rid of R CMD check note: no visible binding for global variable 'manifest_df'
    manifest_df <- NULL
    
  }


  load(manifest_file_name)

  matching_results <- manifest_df[grepl(result_name, manifest_df$result_name), ]


  if (dim(matching_results)[1] == 0) {

    stop(paste("There is no saved result_name that matches the specified result name of: ", result_name))

    # added this line to get rid of R CMD check note: no visible binding for global variable 'DECODING_RESULTS'
    DECODING_RESULTS <- NULL
    
  } else if (dim(matching_results)[1] == 1) {

    # only one result exists so load it and return it
    load(paste0(results_dir_name, matching_results$analysis_ID[1], ".rda"))
    DECODING_RESULTS
    
  } else {

    # more than one result matches the result_name given return all the results in a list

    num_matching_results <- dim(matching_results)[1]
    all_decoding_results <- list()

    for (i in 1:num_matching_results) {
      load(paste0(results_dir_name, matching_results$analysis_ID[i], ".rda"))
      all_decoding_results[[i]] <- DECODING_RESULTS
    }

    # return all the decoding results as a list
    all_decoding_results
    
  }
}






# A helper function that adds the last character of a / or a \ to the directory name
#  if the directory name doesn't contain this character already.
add_last_character_to_directory_name <- function(directory_name) {

  # if the directory name does not end with a slash, add a slash to the directory name
  directory_name <- trimws(file.path(dirname(directory_name), basename(directory_name), " "))

  directory_name
}
