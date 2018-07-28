
create_script <- function(decoding_params) {
  
  
  print(decoding_params)
  
  
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
  
  
  

  
  
  
  
  
  
  
  
  content = readChar(script_full_name, file.info(script_full_name)$size)
  
  print(content)
  return(content)
  
  }  # get the function to create the script file...
