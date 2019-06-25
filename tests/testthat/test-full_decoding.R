

# run_test_full_decoding <- function(cv, plot_results = FALSE) {
#   
#   # DECODING_RESULTS <- cv$run_decoding()
#   ALL_DECODING_RESULTS <- run_decoding(cv)
#   DECODING_RESULTS <- ALL_DECODING_RESULTS[[1]]
#   
#   if (plot_results) {
#     #plot(DECODING_RESULTS$zero_one_loss_results)
#     temp_mean_results <- colMeans(DECODING_RESULTS$zero_one_loss_results)
#     plot(diag(temp_mean_results), type = 'o')
#   }
# 
#   return(DECODING_RESULTS)
#   
# }



basedir_file_name <- '../../data/binned/ZD_150_samples_binned_every_50_samples.Rda'

# R6 versions
#ds <- NDTr::basic_DS$new(basedir_file_name, 'stimulus_ID', 18, 0)
#cl <- NDTr::max_correlation_CL$new()
#fps <- list(NDTr::zscore_FP$new())


ds <- basic_DS(basedir_file_name, 'stimulus_ID', 18, 0)
fps <- list(zscore_FP())
cl <- max_correlation_CL()
cv <- standard_CV(ds, cl, fps, 3) 

DECODING_RESULTS <- run_decoding(cv)

mean_results <- DECODING_RESULTS %>% 
  dplyr::group_by(train_time, test_time) %>%
  mutate(resample_run = as.numeric(resample_run)) %>%
  summarize_all(mean) %>%
  select(-resample_run, -CV)

ggplot2::ggplot(mean_results, aes(test_time, train_time, fill = mean_accuracy)) + 
  geom_tile()


mean_results %>% filter(train_time == test_time) %>%
  ggplot2::ggplot(aes(x = train_time, y = mean_accuracy)) + 
  geom_point() 

# collapsed_results <- dplyr::bind_rows(DECODING_RESULTS, .id = "resample_run")



#DECODING_RESULTS <- run_decoding(cv)



# test if shuffling the results returns results at chance...
# ds <- NDTr::basic_DS$new(basedir_file_name, 'stimulus_ID', 18, 0, randomly_shuffled_labels_before_running = TRUE)
# cv <- NDTr::standard_CV$new(ds, cl, fps) 
# DECODING_RESULTS_SHUFFLED <- run_test_full_decoding(cv)



# 
# 
# 
# basedir_file_name <- '../../data/binned/ZD_150_samples_binned_every_50_samples.Rda'
# 
# 
# ds <- basic_DS(basedir_file_name, 'stimulus_ID', 18, 0)
# fps <- list(zscore_FP())
# cl <- max_correlation_CL()
# cv <- standard_CV(ds, cl, fps, 5) 
# 
# ALL_RESULTS <- run_decoding(cv)
# 
# #set up parallel resources
# cores <- parallel::detectCores()
# the_cluster <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(the_cluster)
# 
# ALL_DECODING_RESULTS <- foreach(iResample = 1:10) %dopar% {
#   #run_decoding(cv)
#   
# }
# 
# for (i in 1:10){
#   run_decoding(cv)
# }
# 
# 
# 
# # results <- foreach(i=1:n, .export=c('function1', 'function2'), .packages='package1') %dopar% {
# #   # do something cool
# # }
# 
# 
# 
# #library(doParallel)
# cores <- parallel::detectCores()
# the_cluster <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(the_cluster)
# foreach(i=1:3) %dopar% sqrt(i)
# 
# stopCluster(the_cluster)
# 


