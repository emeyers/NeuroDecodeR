library('NDTr')

NDTr_name = "~/NDTr"

# should read up about doing real unit testing...


# install("NDTr")
# document()    # sometimes can't find the NDTr code until document has been run
matlab_raster_directory_name = "data/raster/Zhang_Desimone_7objects_raster_data/"
matlab_raster_directory_name ="data/raster/sample_boda_sg/"
create_raster_data_from_matlab_raster_data(matlab_raster_directory_name)


raster_directory_name <- file.path("data/raster", "Zhang_Desimone_7objects_raster_data_rda")
# create_binned_data(raster_directory_name, 'data/binned/ZD', 150, 50)

create_binned_data(raster_directory_name, 'data/binned/ZD', 10, 3)




we rm(list = ls())


# define the decoding parameters...

binned.file.name <- file.path('ZD_binned_data_150ms_bins_50ms_sampled.Rda')
specific.binned.label.name <- "stimulus_ID"    # which labels to decode
num.cv.splits <- 5   # the number of cross-validation splits



# test creating a basic DS
ds <- basic_DS$new(binned.file.name, specific.binned.label.name, num.cv.splits)


# test create a classifier
cl <- max_correlation_CL$new()


# test create a zscore feature preprocessor
fps <- list(zscore_FP$new())

# test create two FPs...
# fps <- list(zscore_FP$new(), select_k_features_FP$new(30))  # using only the top 30 features...


# test create a PNB classifier
# cl <- poisson_naive_bayes_CL$new()
# fps <- NULL  # do not want to use a z-score pre-processor when using a PNB classifier...
# ds <- basic_DS$new(binned.file.name, specific.binned.label.name, num.cv.splits, TRUE)  # want to load the data as counts not rates when using a PNB classifier

# test SVM
#cl <- svm_CL$new()


# set data source parameters to repeat each label 4 times per CV split
ds$num_times_to_repeat_labels_per_cv_block <- 4


# test creating a cross-validator
cv <- standard_CV$new(ds, cl, fps)


# run the decoding analysis...
Rprof(tmp <- tempfile(), line.profiling=TRUE)
DECODING_RESULTS <- cv$run_decoding()
Rprof()
print(summaryRprof(tmp, lines = "show"))


# save the results...
save("DECODING_RESULTS", file = "ZD_MCC_zscore.Rda")







# plot the results...

library('fields')


# different result types that can be plotted
all_results <- DECODING_RESULTS$zero_one_loss_results
all_results <- DECODING_RESULTS$decision_value_results
all_results <- DECODING_RESULTS$rank_results


# get the mean over CV splits
mean_results <- colMeans(all_results)
time_bin_names <- get_center_bin_time(dimnames(all_results)[[3]])



# plot full TCT plot
image.plot(time_bin_names, time_bin_names, mean_results,
           legend.lab = "Classification Accuracy", xlab = "Test time (ms)",
           ylab = "Train time (ms)")
abline(v = 0)



# plot results as a function of time
plot(time_bin_names, diag(mean_results), type = "o", xlab = "Time (ms)", ylab = "Decoding Accuracy")
abline(v = 0)


