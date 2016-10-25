

# This code creates a markdown file that can be run to create decoding results...



create_markdown_file <- function(decoding_params) {

  
  print(decoding_params)
  
  
  markdown_basedir_name <- "markdown_files/"
  
  markdown_file_name <- "decoding_markdown.Rmd"
  
  
  full_file_name <- paste0(markdown_basedir_name, markdown_file_name)
  
  
  # overwrite the file for now while I'm still figure out how to create it...
  #file.create(full_file_name, overwrite = TRUE)
  file.create(full_file_name)
  
  
  # write the header
  write("---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n", 
        file = full_file_name)
  
  
  
    
    # write options for displaying the chunks
    write("```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```\n\n", file = full_file_name, append = TRUE)
    
    

    
# load the necessary files and libraries
write("#Load the necessary packages and files
```{r load_files}
library('tictoc')
library('fields')

base_ndtr_dir_name <- '../../R/'
base_data_dir_name <- '../../data/'

# source all files in the R directory
source(paste0(base_ndtr_dir_name, 'helper_functions.R'))
source(paste0(base_ndtr_dir_name, 'basic_DS.R')) 
source(paste0(base_ndtr_dir_name, 'max_correlation_CL.R')) 
source(paste0(base_ndtr_dir_name, 'poisson_naive_bayes_CL.R'))
source(paste0(base_ndtr_dir_name, 'select_k_features_FP.R')) 
source(paste0(base_ndtr_dir_name, 'zscore_FP.R')) 
source(paste0(base_ndtr_dir_name, 'standard_CV.R')) 
```\n\n\n", file = full_file_name, append = TRUE)
  





# list all the input decoding parameters
#write(paste0("#List all decoding parameters
#```{r input_parameters}

#\n\n print(decoding_params)", 
             
#"\n\n```\n\n\n"), file = full_file_name, append = TRUE)





# 1. write code to create the feature preprocessors 

write("#Create the feature preprocessors
```{r create_fps}

fps <- NULL

```\n\n\n", file = full_file_name, append = TRUE)





# 2. write code to create the classifiers

if (decoding_params$CL.name == "Maximum Correlation") {
  classifier_constructor_text <- "cl <- max_correlation_CL$new()"
}

if (decoding_params$CL.name == "Poisson Naive Bayes") {
classifier_constructor_text <- "cl <- poisson_naive_bayes_CL$new()\n
use.count.data <- TRUE"
}




write(paste0("#Create the classifier
```{r create_classifiers}

use.count.data <- FALSE\n\n", 
             
classifier_constructor_text,


"\n\n```\n\n\n"), file = full_file_name, append = TRUE)






# 3. write code to create the data source
 

file_name_text <- paste0("binned.file.name <- paste0(base_data_dir_name, ", "'", decoding_params$DS.binned_data_name, "'", ')')
ds_text <- paste0("\n\nds <- basic_DS$new(binned.file.name, ", "'", decoding_params$DS.specific_binned_label_names, "'", ", ", decoding_params$DS.num_cv_splits, ", use.count.data)\n\n")
ds_text2 <- paste0("ds$num.times.to.repeat.labels.per.cv.block <- ", decoding_params$DS.num_repeats_per_cv_split)




write(paste0("#Create the data source
```{r create_ds}\n\n",

file_name_text, 

ds_text,

ds_text2,



"\n\n```\n\n\n"), file = full_file_name, append = TRUE)
      


# 4. write code to create the cross-validator

# will need to add this in later...
# # this actually doesn't do anything because currently the cross-validator only does 1 resample run
# #cv$num.resample.runs <- decoding_params$CV.num_resample_runs



write(paste0("#Create the cross-validator
```{r create_cv}\n\n

cv <- standard_CV$new(ds, cl, fps)

DECODING_RESULTS <- cv$run_decoding()
             

save('DECODING_RESULTS', file = 'markdown_results/curr_temp_results.Rda')
             
\n\n```\n\n\n"), file = full_file_name, append = TRUE)




# 5. write code to plot the results


Plot.TCT_result_type_to_plot <- "Zero-one loss"


if (Plot.TCT_result_type_to_plot == "Zero-one loss"){
  all.results_name <- "DECODING_RESULTS$zero.one.loss.results"
  labels_names <- "'Classification Accuracy'"
}

if (Plot.TCT_result_type_to_plot == "Rank results"){
  all.results_name <- "DECODING_RESULTS$decision.value.results"
  labels_names <- "'Normalized rank'"
}

if (Plot.TCT_result_type_to_plot == "Decision Values"){
  all.results_name <- "DECODING_RESULTS$rank.results"
  labels_names <- "'Decision values'"
}




write(paste0("#Plot the results
```{r plot_tct}\n\n

all.results <- ", all.results_name, 

             
"# get the mean over CV splits\n
mean.results <- colMeans(all.results)
time.bin.names <- get.center.bin.time(dimnames(all.results)[[3]])
             
           
# plot full TCT plot
image.plot(time.bin.names, time.bin.names, mean.results, 
           legend.lab = ", labels_names, ", xlab = 'Test time (ms)', 
           ylab = 'Train time (ms)')
abline(v = 0)

  
\n\n```\n\n\n"), file = full_file_name, append = TRUE)















}  # get the function to create the markdown file...


 






