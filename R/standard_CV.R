
# rm(list = ls())

# This will ultimately be an cross-validator S4 object, but for now just a script...


source('get_data_for_decoding.R')
source('max_correlation_CL.R')

library('tictoc')

library('e1071')  # svm package...

# below is the beginning of a cross-validator...


binned.file.name <- "../data/ZD_binned_data_150ms_bins_10ms_sampled.Rda"     
specific.binned.label.name <- "stimulus.ID"    # which labels to decode
num.cv.splits <- 20   # the number of cross-validation splits


tic()
all.neural.data <- get_data_for_decoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
toc()




unique.times <- unique(all.neural.data$time)


# get code to see if it worked...





num.CV <- length(unique(all.neural.data$CV.num))
num.time.bins <- length(unique.times)



all.results <- array(NA, c(num.CV, num.time.bins, num.time.bins))


for (iCV in 1:num.CV)
{
  
  tic()
  print(iCV) 
  
  for (iTrain in 1:num.time.bins)   # training time bin
  {
    
    train.data <- filter(all.neural.data, time == unique.times[iTrain], CV.num != iCV) %>% select(starts_with("X"), labels)
    # test.data <- filter(all.neural.data, time == unique.times[iTime], CV.num == 1) %>% select(starts_with("X"), labels)
    
    # try to do all the times at once...
    #test.data <- filter(all.neural.data, CV.num == 1) %>% select(starts_with("X"), labels)
    test.data <- filter(all.neural.data, CV.num == iCV) 
    
    classifiaction.accuracy <- maxCorrelation_CL(train.data, test.data)
    
    all.results[iCV, iTrain, ] <- classifiaction.accuracy$mean.accuracy
    
  }
  
  toc()
}





# plot full TCT plot

library('fields')

mean.results <- colMeans(all.results)

time.vector <- classifiaction.accuracy$time
time.bin.centers <- get.center.bin.time(time.vector)
sorted.times <- sort(time.bin.centers, index.return = TRUE)


image.plot(sorted.times$x, sorted.times$x, mean.results[, sorted.times$ix], 
           legend.lab = "Classification Accuracy", xlab = "Test time (ms)", 
           ylab = "Train time (ms)")

abline(v = 0)




















# 
# 
# # older stuff
# 
# 
# num.time.bins <- length(unique(all.neural.data$time))
# 
# test.only.at.training.times <- TRUE
# 
# if (test.only.at.training.times) {
#   all.results <- array(NA, c(num.time.bins, 20))
# }else{
#   all.results <- array(NA, c(num.time.bins, num.time.bins, 20))
# }
# 
# 
# for (iCV in 1:20) 
# {
#   
#   tic()
#   print(iCV)
#   
#   for (iTrain in 1:num.time.bins) 
#   {
#     
#     
#     neural.data.train <- filter(all.neural.data, time == unique.times[iTrain])
#     train.data <- filter(neural.data.train, CV.num != iCV) %>% select(starts_with("X"), labels)
#     
#      svm.fit <- svm(labels ~., data = train.data, kernel = "linear", scale = TRUE)
#     #svm.fit <- svm(labels ~., data = train.data, kernel = "linear", scale = FALSE)
#     
#     class.accuracy <- NULL
#     
#     
#     if (test.only.at.training.times){
#       test.times <- iTrain
#     }else{
#       test.times <- 1:num.time.bins
#     }
#     
#     
#     for (iTest in test.times)
#     {
#       
#       neural.data.test <- filter(all.neural.data, time == unique.times[iTest])
#       test.data <- filter(neural.data.test, CV.num == iCV) %>% select(starts_with("X"), labels)
#       
#       curr.result <- sum(predict(svm.fit, newdata = test.data) == test.data$labels)/dim(test.data)[1]
#       
#       if (test.only.at.training.times){
#         all.results[iTrain, iCV] <- curr.result
#       }else{
#         all.results[iTrain, iTest, iCV] <- curr.result
#       }
#       
#       
#       #lda.fit <- lda(labels ~., data = train.data)
#       #sclass.accuracy[iCV] <- sum(predict(lda.fit, newdata = test.data)$class == test.data$labels)/dim(test.data)[1]
#       
#     }
#     
#     
#     #all.results <- rbind(all.results, class.accuracy)
#     #print(all.results)   # print results up to current time
#     
#     
#   }
#   
#   toc()
#   
# }
# 
# 
# 
# if (test.only.at.training.times) {
#   all.mean.results <- rowMeans(all.results)
# }else{
#   all.mean.results <- apply(all.results, c(1,2), mean)
# }
# 
# 
# # not sure this is working...
# 
# 
# plot(rowMeans(all.results), type = 'o', ylab = "Classification Accuracy")
# abline(h = 1/7, col = 'red')
# 
# 
# if (!test.only.at.training.times)
# {
#   library('fields')
#   image.plot(all.mean.results)
# }













# very old junk


#all.mean.results <- matrix(NA, 18, 18)
#for (iTrain in 1:18)
#{
#  for (iTest in 1:18)
#  {
#    all.mean.results[iTrain, iTest] <- mean(all.results[iTrain, iTest, ])
#  }
#}

#image(all.mean.results)


# banding on the off diagonal because all CVs don't come from the same trials :(

#plot(seq(75, 925, by = 50) - 500, all.mean.results, xlab = "Time (ms)", ylab = "Classification Accuracy", type = "o")
#abline(h = 1/7)
#abline(v = 0)



# 
# 
# 
# 
# # get code to see if it worked...
# 
# all.mean.results <- NULL
# all.results <- NULL
# for (iTime in 1:18)
# {
#   
#   print(iTime)
#   
#   tic()
#   neural.data <- filter(all.neural.data, time == unique.times[iTime])
#   #neural.data <- getDataForDecoding(iTime)
#   
#   class.accuracy <- NULL
#   for (iCV in 1:20)
#   {
#     test.data <- filter(neural.data, CV.num == iCV)[, 1:133]
#     train.data <- filter(neural.data, CV.num != iCV)[, 1:133]
#     
#     svm.fit <- svm(labels ~., data = train.data, kernel = "linear", scale = TRUE)
#     class.accuracy[iCV] <- sum(predict(svm.fit, newdata = test.data) == test.data$labels)/dim(test.data)[1]
#     
#     #lda.fit <- lda(labels ~., data = train.data)
#     #sclass.accuracy[iCV] <- sum(predict(lda.fit, newdata = test.data)$class == test.data$labels)/dim(test.data)[1]
#     
#     
#   }
#   
#   all.mean.results[iTime] <- mean(class.accuracy)
#   all.results <- rbind(all.results, class.accuracy)
#   
#   #print(all.results)   # print results up to current time
#   
#   toc()
#   
# }
# 
# plot(seq(75, 925, by = 50) - 500, all.mean.results, xlab = "Time (ms)", ylab = "Classification Accuracy", type = "o")
# abline(h = 1/7)
# abline(v = 0)
# 

























