


# doing ANOVAs to test how many sites are selective...


library('tictoc')



source('getDataForDecoding.R')


 binned.file.name <- "ZD_binned_data_150ms_bins_50ms_sampled.Rda"     
# binned.file.name <- "ZD_binned_data_150ms_bins_10ms_sampled.Rda"     

specific.binned.label.name <- "stimulus.ID"    # which labels to decode
num.cv.splits <- 20   # the number of cross-validation splits

tic()
all.neural.data <- getDataForDecoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
toc()


# run the code with a profiler on to see how long it takes...
Rprof(tmp <- tempfile(), line.profiling=TRUE)
all.neural.data <- getDataForDecoding(binned.file.name, specific.binned.label.name, num.cv.splits)  
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












