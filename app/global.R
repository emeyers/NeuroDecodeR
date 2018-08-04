
library('fields')
library('ggplot2')
library("plotrix")

rm(list=ls())

# working directoy is initialized as the dir of run file
# setwd("..")
setwd("C:/Users/14868/Documents/GitHub/NDTr")
# setwd("/cbcl/cbcl01/xf15/NDTr")

# all_cl <- c("maximum correlation", "support vecotor machine", "poisson naive bayes")
# all_fp <- c("select_pvalue_significant_features","select or exclude top k features", "zscore_normalize")

# df_cl_fp <- data.frame(c(1, 1, 1), c(1, 1, 1), c(1, 1, 0))

all_cl <- c("maximum_correlation_CL", "svm_CL", "poisson_naive_bayes_CL")
all_fp <- c("select_k_features_FP", "zscore_FP")

df_cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))

colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp

req_dc_para <- c("CL", "CV_bDiag", "CV_repeat", "CV_resample", "CV_split", "DS_chosen_bin", "DS_type")

# req_dc_para_basic <-c("DS_bUse_all_levels")
#
# req_dc_para_basic_leve <- c()

all_input <- list()

input_id <- c("bin_create_raster", "bin_bCreate_raster_in_rda",
              "bin_bin_data", "bin_bin_width", "bin_bPlot", "bin_chosen_raster",
              "bin_end_ind", "bin_next_neuron", "bin_prefix_of_binned_file_name",
              "bin_new_raster", 
              "bin_pre_neuron", 
              "bin_start_ind", "bin_step_size",
  "bin_uploaded_raster", "CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
  "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample",
  "CV_split", "DC_run_decoding", "DC_save_script", "DC_scriptize",
  "DC_upload", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels",
  "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",
  "DS_gen_var_to_use", "DS_type", "DS_uploaded_bin", "FP", "FP_excluded_k",
  "FP_selected_k", "Plot_basic_result_type_to_plot", "Plot_TCT_result_type_to_plot",
  "script", "sidebarCollapsed", "sidebarItemExpanded")


input_label <- c("Create raster", "We can bin raster data in .mat format, but do you want to create raster data in .Rda format? Benefits include the option to plot raster data ",
                 "Bin the data", "Bin width", "Plot the data? (only for spike trains in .Rda file)", "Choose raster data directory",
                 "Index of the time where the last bin ends (optional)", "next file","prefix of binned file name (e.g., data/binned/ZD_)",
                 "New raster directory name (e.g., data/raster/Zhang_Desimone_7objects_raster_data_rda; by default, we append '_rda' to the matlab raster directory name)",
                 "previous file", 
                 "Index of the time where the first bin starts (optional)", "Step size",
                 "Upload new raster data (optional; only accept .mat and .Rda formmat)", "Classifier", "Coef0", "Cost", "Degree of polynomial",
                 "Gamma", "Kernel", "Test only at training times?", "Number of repeats of each level in each CV split", "Number of resampling runs",
                 "Number of cross validation split", "Run decoding", "Save the script", "Generate script from gui configuration",
                 "Upload new script (optional)", "Levels to use", "Variable to decode and to use", "Use all the levels of this variable?",
                 "Choose your binned data", "How many training level groups you will use?",  "Variable to decode",
                 "Variable to use", "Type of data source", "Upload new binned data (optional)", "Feature Preprocessors", "exclude top ? features (this will be applied second)",
                 "select top ? features (this will be applied first)", "Type of result to plot", "Type of result to plot",
                 NA, NA, NA)

lLabel <- as.list(input_label)
names(lLabel) <- input_id

binning_paras <- paste0("input$",c("bin_bin_width","bin_chosen_raster","bin_end_ind","bin_start_ind", "bin_step_size"))


temp_decoding_paras_id <<- c("CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
                             "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample",
                             "CV_split", "CV_repeat", "CV_resample", "CV_split", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels",
                             "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",
                             "DS_gen_var_to_use", "DS_type","FP", "FP_excluded_k",
                             "FP_selected_k")

temp_decoding_paras_input_id <<- paste0("input$", temp_decoding_paras_id)
