rm(list=ls())

setwd("C:/Users/14868/Documents/GitHub/NDTr")
# setwd("/cbcl/cbcl01/xf15/NDTr")

all_cl <- c("maximum correlation", "support vecotor machine", "poisson naive bayes")
all_fp <- c("select_pvalue_significant_features","select or exclude top k features", "zscore_normalize")

df_cl_fp <- data.frame(c(1, 1, 1), c(1, 1, 1), c(1, 1, 0))
colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp

req_dc_para <- c("CL", "CV_bDiag", "CV_repeat", "CV_resample", "CV_split", "DS_chosen_bin", "DS_type", "FP")

# req_dc_para_basic <-c("DS_bUse_all_levels")
# 
# req_dc_para_basic_leve <- c()

all_input <- list()

inpud_id <- c("bin_bin_data", "bin_bin_width", "bin_chosen_raster", "bin_step_size", 
  "bin_uploaded_raster", "CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree", 
  "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample", 
  "CV_split", "DC_run_decoding", "DC_save_script", "DC_scriptize", 
  "DC_upload", "DS_basic_var_to_decode", "DS_bUse_all_levels", 
  "DS_chosen_bin", "DS_type", "DS_uploaded_bin", "FP", "FP_excluded_k", 
  "FP_selected_k", "Plot_basic_result_type_to_plot", "Plot_TCT_result_type_to_plot", 
  "script", "sidebarCollapsed", "sidebarItemExpanded")

