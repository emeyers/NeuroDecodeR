rm(list=ls())

setwd("C:/Users/14868/Documents/GitHub/NDTr")
# setwd("/cbcl/cbcl01/xf15/NDTr")

# all_cl <- c("maximum correlation", "support vecotor machine", "poisson naive bayes")
# all_fp <- c("select_pvalue_significant_features","select or exclude top k features", "zscore_normalize")

# df_cl_fp <- data.frame(c(1, 1, 1), c(1, 1, 1), c(1, 1, 0))

all_cl <- c("maximum correlation", "support vecotor machine", "poisson naive bayes")
all_fp <- c("select or exclude top k features", "zscore_normalize")

df_cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))

colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp

req_dc_para <- c("CL", "CV_bDiag", "CV_repeat", "CV_resample", "CV_split", "DS_chosen_bin", "DS_type")

# req_dc_para_basic <-c("DS_bUse_all_levels")
# 
# req_dc_para_basic_leve <- c()

all_input <- list()

input_id <- c("bin_bin_data", "bin_bin_width", "bin_chosen_raster", "bin_step_size", 
  "bin_uploaded_raster", "CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree", 
  "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample", 
  "CV_split", "DC_run_decoding", "DC_save_script", "DC_scriptize", 
  "DC_upload", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels", 
  "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",            
  "DS_gen_var_to_use", "DS_type", "DS_uploaded_bin", "FP", "FP_excluded_k", 
  "FP_selected_k", "Plot_basic_result_type_to_plot", "Plot_TCT_result_type_to_plot", 
  "script", "sidebarCollapsed", "sidebarItemExpanded")


input_label <- c("Bin the data", "Bin width", "Choose your raster data", "Step size",
                 "Upload new raster data (optional)", "Classifier", "Coef0", "Cost", "Degree of polynomial",
                 "Gamma", "Kernel", "Test only at training times?", "Number of repeats of each level in each CV split", "Number of resampling runs",
                 "Number of cross validation split", "Run decoding", "Save the script", "Generate script from gui configuration",
                 "Upload new script (optional)", "Levels to use", "Variable to decode and to use", "Use all the levels of this variable?",
                 "Choose your binned data", "How many training level groups you will use?",  "Variable to decode",
                 "Variable to use", "Type of data source", "Upload new binned data (optional)", "Feature Preprocessors", "exclude top ? features (this will be applied second)",
                 "select top ? features (this will be applied first)", "Type of result to plot", "Type of result to plot",
                 NA, NA, NA)

lLabel <- as.list(input_label)
names(lLabel) <- input_id