#' @export
create_script <- function(my_decoding_paras) {
  
  all_my_inputs <<- names(my_decoding_paras)
  
  my_inputs_global <<- my_decoding_paras
  
  print(names(my_decoding_paras))
  
  
  script_dir_name <- "scripts"
  
  script_name <- "decoding_script.Rmd"
  
  
  script_full_name <- file.path(getwd(),script_dir_name, script_name)
  
  
  # overwrite the file for now while I'm still figure out how to create it...
  #file.create(script_full_name, overwrite = TRUE)
  file.create(script_full_name)
  
  my_text = ""
  # write the header
  # write("---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n", 
  # file = script_full_name)
  
  # my_text = paste0(my_text, "---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n",
  #                  "```{r setup, include=FALSE}\n",
  #                  "knitr::opts_chunk$set(echo = TRUE)\n",
  #                  "```\n")
  # 
  
  # my_text = paste0(my_text, "\n```{r}\n")
  
  
  if(my_decoding_paras$DS_type == "basic_DS"){
    my_text = paste0(my_text, "binned_data_file_name <-", my_decoding_paras$DS_chosen_bin, "\n")
    my_text = paste0(my_text, "variable_to_decode <-", my_decoding_paras$DS_basic_var_to_decode, "\n")
    my_text = paste0(my_text, "num_cv_split <- ", my_decoding_paras$CV_split, "\n")
    
    my_text = paste0(my_text, "ds <- basic_DS$new(binned_file_name, specific_binned_label_name, num_cv_splits)\n")
    
  }
  # my_text = paste0(my_text, "```\n")
  
  # my_text = paste0(my_text, "\n```{r}\n")
  
  #! need to change this basic_DS
  if(!is.null(my_decoding_paras$CV_repeat)){
    my_text = paste0(my_text, "ds$num_repeats_per_level_per_cv_split <- ", my_decoding_paras$CV_repeat, "\n")
  }
  
  
  my_text = paste0(my_text, "cl <- ", my_decoding_paras$CL, "$new()\n")
  
  
  # my_text = paste0(my_text, "```\n")
  
  # my_text = paste0(my_text, "\n```{r}\n")
  
  
  my_text = paste0(my_text, "fps <-list (")
  
  if(!is.null(my_decoding_paras$FP)){
    if(grepl(my_decoding_paras$FP, all_fp[2]) == TRUE){
      my_text = paste0(my_text, my_decoding_paras$FP[2], "$new()")
      
    } else if (grepl(my_decoding_paras$FP, all_fp[1]) == TRUE){
      my_text = paste0(my_text, my_decoding_paras$FP[1], "$new(")
      if(!is.null(my_decoding_paras$FP_selected_k)){
        my_text = paste0(my_text, "num_sites_to_use = ", my_decoding_paras$FP_selected_k)
      } else if(is.null(my_decoding_paras$FP_excluded_k)){
        my_text = paste0(my_text, "num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k)
      } 
      
      if(is.null(my_decoding_paras$FP_excluded_k)){
        my_text = paste0(my_text, ", num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k)
      }
    } 
    
    if(grepl(my_decoding_paras$FP, all_fp[1]) == TRUE){
      my_text = paste0(my_text, ",", my_decoding_paras$FP[2])
      if(!is.null(my_decoding_paras$FP_selected_k)){
        my_text = paste0(my_text, "num_sites_to_use = ", my_decoding_paras$FP_selected_k)
      } else if(is.null(my_decoding_paras$FP_excluded_k)){
        my_text = paste0(my_text, "num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k)
      } 
      
      if(is.null(my_decoding_paras$FP_excluded_k)){
        my_text = paste0(my_text, ", num_sites_to_exclude = ", my_decoding_paras$FP_excluded_k)
      }
    }
  }
  
  my_text = paste0(my_text, ")\n")
  
  # my_text = paste0(my_text, "```\n")
  
  # my_text = paste0(my_text, "\n```{r}\n")
  
  my_text = paste0(my_text, "cv <- standard_CV$new(ds, cl, fps)\n",
                   "DECODING_RESULTS <- cv$run_decoding()")
  
  # my_text = paste0(my_text, "```\n")
  
  
  #   write("#Load the necessary packages and files
  # ```{r load_files}
  #         library('tictoc')
  #         library('fields')
  #         base_ndtr_dir_name <- '../R/'
  #         base_data_dir_name <- '../data/'
  #         # source all files in the R directory
  #         source(paste0(base_ndtr_dir_name, 'helper_functions.R'))
  #         source(paste0(base_ndtr_dir_name, 'basic_DS.R')) 
  #         source(paste0(base_ndtr_dir_name, 'max_correlation_CL.R')) 
  #         source(paste0(base_ndtr_dir_name, 'poisson_naive_bayes_CL.R'))
  #         source(paste0(base_ndtr_dir_name, 'select_k_features_FP.R')) 
  #         source(paste0(base_ndtr_dir_name, 'zscore_FP.R')) 
  #         source(paste0(base_ndtr_dir_name, 'standard_CV.R')) 
  #         ```\n\n\n", file = script_full_name, append = TRUE)
  #   
  
  
  
  
  
  # content = readChar(script_full_name, file.info(script_full_name)$size)
  
  return(my_text)
  
}  # get the function to create the script file...
