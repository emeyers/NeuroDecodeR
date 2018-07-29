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

lapply(req_dc_para, function(i){
  paste0(i, "O")
})




