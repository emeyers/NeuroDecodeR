library(NDTr)
library(testthat)



matlab_raster_dir <- "data/raster/Zhang_Desimone_7objects_raster_data/"
raster_dir <- file.path("data/raster", "Zhang_Desimone_7objects_raster_data_rda")
create_binned_data(raster_directory_name, 'data/binned/ZD', 150, 50)

rm(list = ls())

binned.file.name <- file.path('data/binned','ZD_binned_data_150ms_bins_50ms_sampled.Rda')
specific.binned.label.name <- "stimulus_ID"    # which labels to decode
num.cv.splits <- 5   # the number of cross-validation splits

# test creating a basic DS
load_all()
ds <- basic_DS$new(binned.file.name, specific.binned.label.name, num.cv.splits)
the_data <- ds$get_data()
