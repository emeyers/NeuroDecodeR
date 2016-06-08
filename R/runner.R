


# rm(list = ls())



source('basic_DS.R')   # uses Winston's R6 system
source('max_correlation_CL.R')
source('standard_CV.R')
source('zscore_FP.R')
#source('svm_CL.R')


library('tictoc')





# define the decoding parameters...

binned.file.name <- "../data/ZD_binned_data_150ms_bins_10ms_sampled.Rda"     
specific.binned.label.name <- "stimulus.ID"    # which labels to decode
num.cv.splits <- 20   # the number of cross-validation splits



# create the relevant objects and run the decoding...

ds <- basic_DS$new(binned.file.name, specific.binned.label.name, num.cv.splits)


cl <- max_correlation_CL$new()
#cl <- svm_CL$new()
#cl <- probsvm_CL$new()


fp1 <- zscore_FP$new()

fps <- list(fp1)

cv <- standard_CV$new(ds, cl, fps)


Rprof(tmp <- tempfile(), line.profiling=TRUE)
DECODING_RESULTS <- cv$run_decoding()
Rprof()
summaryRprof(tmp, lines = "show")




# save("DECODING_RESULTS", file = "results/MCC_zscore.Rda")



# plot the results...

library('fields')



# different result types that can be plotted
all.results <- DECODING_RESULTS$zero.one.loss.results
all.results <- DECODING_RESULTS$decision.value.results
all.results <- DECODING_RESULTS$rank.results


# get the mean over CV splits
mean.results <- colMeans(all.results)
time.bin.names <- get.center.bin.time(dimnames(all.results)[[3]])



# plot full TCT plot
image.plot(time.bin.names, time.bin.names, mean.results, 
           legend.lab = "Classification Accuracy", xlab = "Test time (ms)", 
           ylab = "Train time (ms)")
abline(v = 0)



# plot results as a function of time
plot(time.bin.names, diag(mean.results), type = "o", xlab = "Time (ms)", ylab = "Decoding Accuracy")
abline(v = 0)







# old...

# 
# time.vector <- classifiaction.accuracy$time
# time.bin.centers <- get.center.bin.time(time.vector)
# sorted.times <- sort(time.bin.centers, index.return = TRUE)
# 
# 
# image.plot(sorted.times$x, sorted.times$x, mean.results[, sorted.times$ix], 
#            legend.lab = "Classification Accuracy", xlab = "Test time (ms)", 
#            ylab = "Train time (ms)")
# 
# abline(v = 0)























