
source('helper_functions.R')


# the MCC classifier - could clean up, and potentially speed up the code (and make into an object)

maxCorrelation_CL <- function(train.data, all.times.test.data)
{
  
  prototypes <- train.data %>% group_by(labels) %>% summarise_each(funs(mean))
  
  
  train.test.cor <- cor(t(prototypes[, 2:133]), t(select(all.times.test.data, -labels, -time, -CV.num)))
  
  
  predicted.inds <- apply(train.test.cor, 2, which.max)   # need to create rand.which.max() function...
  predicted.labels <- prototypes$labels[predicted.inds]
  
  results <- data.frame(time = all.times.test.data$time, actual.labels = all.times.test.data$labels, predicted.labels = predicted.labels) %>%
    mutate(correct = actual.labels == predicted.labels)
  
  classifiaction.accuracy <- results %>% group_by(time) %>% summarize(mean.accuracy = mean(correct))
  
  return(classifiaction.accuracy)
  
}









# 
# # code to test the classifier...
# 
# 
# binned.file.name <- "ZD_binned_data_150ms_bins_10ms_sampled.Rda"     
# specific.binned.label.name <- "stimulus.ID"    # which labels to decode
# num.cv.splits <- 20   # the number of cross-validation splits
# 
# 
# tic()
# all.neural.data <- getDataForDecoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
# toc()
# 
# 
# 
# 
# tic()
# 
# 
# unique.times <- unique(all.neural.data$time)
# 
# 
# 
# 
# #plot(time.bin.centers, colMeans(all.results))
# 






#   # training the classifier...
#   prototypes <- train.data %>% group_by(labels) %>% summarise_each(funs(mean))
#   
#   
#   
#   train.test.cor <- cor(t(prototypes[, 2:133]), t(select(test.data, -labels, -time, -CV.num)))
#   
#   
#   predicted.inds <- apply(train.test.cor, 2, which.max)
#   predicted.labels <- prototypes$labels[predicted.inds]
#   
#   results <- data.frame(time = test.data$time, actual.labels = test.data$labels, predicted.labels = predicted.labels) %>%
#     mutate(correct = actual.labels == predicted.labels)
#   
#   classifiaction.accuracy <- results %>% group_by(time) %>% summarize(mean.accuracy = mean(correct))
#   
#   
#   all.results[iCV, ] <- classifiaction.accuracy$mean.accuracy
#   









  

#sum(predicted.labels == prototypes$labels)/nrow(test.data)





# library('fields')
# image.plot(blah)




# 
# 
# maxCorrelation_CL <- setClass(
#   
#   # Set the name for the class
#   "maxCorrelation_CL",
#   
#   # Define the slots
#   slots = c(
#     
#     trainingPrototypes = "numeric",
#     topOfInning = "logical",
#     outs = "numeric",
#     balls = "numeric",
#     strikes = "numeric",
#     
#     runnerOnFirst = "logical",
#     runnerOnSecond = "logical",
#     runnerOnThird = "logical",
#     
#     homeTeamBatterNumber = "numeric", 
#     visitingTeamBatterNumber = "numeric",
#     
#     homeTeamScore = "numeric",
#     visitingTeamScore = "numeric",
#     
#     gameIsOver = "logical"
#     
#   ),
#   
#   # Set the default values for the slots. (optional)
#   prototype=list(
#     
#     inning = 1,
#     topOfInning = TRUE,
#     outs = 0,
#     balls = 0,
#     strikes = 0,
#     
#     runnerOnFirst = FALSE,
#     runnerOnSecond = FALSE,
#     runnerOnThird = FALSE,
#     
#     homeTeamBatterNumber = 1, 
#     visitingTeamBatterNumber = 1, 
#     
#     homeTeamScore = 0,
#     visitingTeamScore = 0, 
#     
#     gameIsOver = FALSE
#     
#   ),
#   
#   # Make a function that can test to see if the data is consistent
#   # This is not called if you have an initialize function defined!
#   # Sort of pointless because this only checks when the object is constructed
#   validity=function(object)
#   {
#     if((object@strikes < 0) || (object@strikes > 2)) {
#       return("The number of strikes must be between 0 and 2")
#     }
#     if((object@balls < 0) || (object@balls > 3)) {
#       return("The number of balls must be between 0 and 3")
#     }    
#     
#     return(TRUE)
#   }
# )
# 
# 
# 
# 
# # create a method to update the game based on what happened on the current pitch
# setGeneric(name="updateGame",
#            def=function(theObject,playName)
#            {
#              standardGeneric("updateGame")
#            }
# )
# 
# 
# # create a method to update the game based on what happened on the current pitch
# setGeneric(name="displayGame",
#            def=function(theObject,playName)
#            {
#              standardGeneric("displayGame")
#            }
# )
# 
# 
# 
# 
# setMethod(f="updateGame",
#           signature="GameState",
#           definition=function(theObject,playName)
#           {
# 
# 
# 
# 
# 
# 
# 
# 










