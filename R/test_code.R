


# just contains a bunch of junk, should remove this from the repository...


# doing ANOVAs to test how many sites are selective...


library('tictoc')



binned.file.name <- "../data/ZD_binned_data_150ms_bins_10ms_sampled.Rda"     
# binned.file.name <- "../data/ZD_binned_data_150ms_bins_50ms_sampled.Rda"     

specific.binned.label.name <- "stimulus.ID"    # which labels to decode
num.cv.splits <- 20   # the number of cross-validation splits



# comparing S4, RC and R6 implementations...








# run the code with a profiler on to see how long it takes...
Rprof(tmp <- tempfile(), line.profiling=TRUE)
all.neural.data <- get_data_for_decoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
Rprof()
summaryRprof(tmp, lines = "show")



# using the lineprof package to profile my code - bad: very hard to read :(
# source('getDataForDecoding.R')
# library(lineprof)
# l <- lineprof(getDataForDecoding(binned.file.name, specific.binned.label.name, num.cv.splits))
# shine(l)



unique.times <- unique(all.neural.data$time)
unique.sites <- names(select(all.neural.data, starts_with("X")))


all.ANOVA.results <- matrix(NA, nrow = length(unique.sites), ncol = length(unique.times))
for (iSite in 1:length(unique.sites)) 
{
  
  print(iSite)
  
  
  for (iTime in 1:length(unique.times)) 
  {
    curr.data <- all.neural.data %>% filter(time == unique.times[iTime]) %>% select(iSite, labels)
    
    blah <- anova(lm(curr.data[, 1] ~ curr.data[, 2]))
    all.ANOVA.results[iSite, iTime] <- blah$`Pr(>F)`[1]
  }
}

num.significant.sites <- apply(all.ANOVA.results < .05, 2, mean, na.rm = TRUE)

plot(num.significant.sites, type = 'o', ylab = "proportion of sites significant", xlab = "time")
abline(h = .05, col = "red")





# can parse the times using...

load(binned.file.name)
binned.data <- select(binned.data, starts_with('time'))
binned.data.names <- names(binned.data)

ave_bin_size <- NULL
for (i in 1:18){
  curr.parsed_names <- unlist(strsplit(binned.data.names[i], ".", fixed = TRUE))
  ave_bin_size[i] <- mean(as.numeric(curr.parsed_names[2:3]))    # length(curr.parsed_names[2]:curr.parsed_names[3])
}

ave_bin_size

# this is what I should use for plotting labels...
ceiling(ave_bin_size)





# 1.8 seconds in MATLAB - using the reshape2 package I'm not getting similar times in R




# comparing S4, RC and R6 objects...


binned.file.name <- "../data/ZD_binned_data_150ms_bins_10ms_sampled.Rda"     
# binned.file.name <- "../data/ZD_binned_data_150ms_bins_50ms_sampled.Rda"     

specific.binned.label.name <- "stimulus.ID"    # which labels to decode
num.cv.splits <- 20   # the number of cross-validation splits



source('basic_DS.R')   # uses Winston's R6 system
ds <- basic_DS$new(binned.file.name, specific.binned.label.name, num.cv.splits)
tic()
all.neural.data <- ds$get_data()
toc()


# RC
source('alternatives/basic_DS_RC_v2.R')
# if using the reference class object you need to name all the input values it seems...
ds <- basic_DS_RC$new(binned.file.name = binned.file.name, specific.binned.label.name = specific.binned.label.name, num.cv.splits = num.cv.splits)
tic()
all.neural.data <- ds$get_data()
toc()


# S4
source('alternatives/basic_DS_S4_v2.R')
ds <- new("basic_DS_S4", binned.file.name = binned.file.name, specific.binned.label.name = specific.binned.label.name, num.cv.splits = num.cv.splits)
tic()
all.neural.data <- get_data(ds)
toc()


# very old version...
source('get_data_for_decoding.R')
tic()
all.neural.data <- get_data_for_decoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
toc()






# junk

# sort.row <- function(decision.val.row) {
#   curr.sorted.decision.vals <- sort(decision.val.row, decreasing = TRUE) 
#   names(curr.sorted.decision.vals)
# }
# tic()
# blah <- apply(decision.vals, 1, sort.row)
# toc()



