
NDTr_name = "/c/Users/14868/Documents/GitHub/NDTr"

# should read up about doing real unit testing...


# install("NDTr")
# document()    # sometimes can't find the NDTr code until document has been run




raster_directory_name <- file.path(NDTr_name,"data", "Zhang_Desimone_7objects_R_raster_data")
create_binned_data(raster_directory_name, 'ZD', 150, 50)





rm(list = ls())


# define the decoding parameters...

binned.file.name <- file.path(NDTr_name, 'data', 'ZD_binned_data_150ms_bins_50ms_sampled.Rda')
specific.binned.label.name <- "stimulus.ID"    # which labels to decode
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
ds$num.times.to.repeat.labels.per.cv.block <- 4


# test creating a cross-validator
cv <- standard_CV$new(ds, cl, fps)


# run the decoding analysis...
Rprof(tmp <- tempfile(), line.profiling=TRUE)
DECODING_RESULTS <- cv$run_decoding()
Rprof()
print(summaryRprof(tmp, lines = "show"))


# save the results...
save("DECODING_RESULTS", file = "results/ZD_MCC_zscore.Rda")







# plot the results...

library('fields')


# different result types that can be plotted
all.results <- DECODING_RESULTS$zero.one.loss.results
all.results <- DECODING_RESULTS$decision.value.results
all.results <- DECODING_RESULTS$rank.results


# get the mean over CV splits
mean.results <- colMeans(all.results)
# time.bin.names <- get.center.bin.time(dimnames(all.results)[[3]])
# 
# 
# 
# # plot full TCT plot
# image.plot(time.bin.names, time.bin.names, mean.results,
#            legend.lab = "Classification Accuracy", xlab = "Test time (ms)",
#            ylab = "Train time (ms)")
# abline(v = 0)
# 
# 
# 
# # plot results as a function of time
# plot(time.bin.names, diag(mean.results), type = "o", xlab = "Time (ms)", ylab = "Decoding Accuracy")
# abline(v = 0)


