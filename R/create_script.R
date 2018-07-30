#' @export
create_script <- function(my_input) {
  
  all_my_input <<- names(my_input)
  
  my_input_global <<- my_input
  
  print(names(my_input))
  
  
  script_dir_name <- "scripts"
  
  script_name <- "decoding_script.Rmd"
  
  
  script_full_name <- file.path(getwd(),script_dir_name, script_name)
  
  
  # overwrite the file for now while I'm still figure out how to create it...
  #file.create(script_full_name, overwrite = TRUE)
  file.create(script_full_name)
  
  
  # write the header
  write("---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n", 
        file = script_full_name)
  
  
  
  
  # write options for displaying the chunks
  write("```{r setup, include=FALSE}
        knitr::opts_chunk$set(echo = TRUE)
        ```\n\n", file = script_full_name, append = TRUE)
  if(my_input$DS_tpye == "basic_DS"){
    print("YEAH")
  }
  
  

  
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
  

  
  
  
  content = readChar(script_full_name, file.info(script_full_name)$size)
  
  print(content)
  return(content)
  
  }  # get the function to create the script file...
