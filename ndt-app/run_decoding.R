

# source relevant decoding files...

library('tictoc')

base_ndtr_dir_name <- '../R/'
base_data_dir_name <- '../data/'

# source all files in the R directory
source(paste0(base_ndtr_dir_name, "helper_functions.R"))
source(paste0(base_ndtr_dir_name, "basic_DS.R")) 
source(paste0(base_ndtr_dir_name, "max_correlation_CL.R")) 
source(paste0(base_ndtr_dir_name, "poisson_naive_bayes_CL.R"))
source(paste0(base_ndtr_dir_name, "select_k_features_FP.R")) 
source(paste0(base_ndtr_dir_name, "zscore_FP.R")) 
source(paste0(base_ndtr_dir_name, "standard_CV.R")) 


# source(paste0(base_ndtr_dir_name, "create_binned_data_from_matlab_raster_data.R"))             
# source(paste0(base_ndtr_dir_name, "svm_CL.R")) 



# lapply(paste0(base_ndtr_dir_name, list.files(base_ndtr_dir_name, pattern = '*.R')), source)



run_decoding <- function(decoding_params) {
  
  
 # print(decoding_params$DS.binned_data_name)
 # print(decoding_params$DS.specific_binned_label_names) 
 # print(decoding_params$DS.num_cv_splits)

  # print(decoding_params$CL.name)
  # print(decoding_params$CV.num_resample_runs)           
  # print(decoding_params$DS.binned_data_name)           
  # print(decoding_params$DS.name)                  
  # print(decoding_params$DS.num_cv_splits)               
  # print(decoding_params$DS.num_repeats_per_cv_split) 
  # print(decoding_params$DS.specific_binned_label_names) 
  
  #print(names(decoding_params))
  
    
  
  # steps need to be done in this order because  if using PNB classifier need to make
  #   sure count data is loaded and z_score proprocessor is not used...
  
  
  # 1. create the feature pre-processors 
  
  # fps <- list(zscore_FP$new(), select_k_features_FP$new(30)) 
 
  fps <- NULL
  
  
  
  

  # 2. create the classifier
  
  # eval(parse(text= paste0("cl <- ", decoding_params$CL.name, "$new()")))
  
  use.count.data <- FALSE
  
  if (decoding_params$CL.name == "Maximum Correlation") {
    cl <- max_correlation_CL$new()
    print("max cl")
  }
  
  if (decoding_params$CL.name == "Poisson Naive Bayes") {
    cl <- poisson_naive_bayes_CL$new()
    use.count.data <- TRUE
    print("PNB")
  }
  
  
  print(decoding_params$CL.name)
  
  
  
  # 3. create the data source
  
  binned.file.name <- paste0(base_data_dir_name, decoding_params$DS.binned_data_name)   # get the full path to the data      
  
  ds <- basic_DS$new(binned.file.name, decoding_params$DS.specific_binned_label_names, decoding_params$DS.num_cv_splits, use.count.data)
  
  ds$num.times.to.repeat.labels.per.cv.block <- decoding_params$DS.num_repeats_per_cv_split
  
  
  
  
  
  # 4. create the cross-validator
  
  cv <- standard_CV$new(ds, cl, fps)
  
  
  # Rprof(tmp <- tempfile(), line.profiling=TRUE)
  DECODING_RESULTS <- cv$run_decoding()
  # Rprof()
  # print(summaryRprof(tmp, lines = "show"))
  
  
  # save the results...
  
  
  save("DECODING_RESULTS", file = "results/curr_temp_results.Rda")
  
  
  
  
  
  
  
}























