#' Convert data from raster format to binned format
#'
#' This function takes the name of a directory that contains files in raster
#' format and averages the data within a specified bin width at specified
#' sampling interval increments to create data in binned format used for
#' decoding.
#'
#'
#' @param raster_dir_name A string that contains the path to a directory that
#'   has files in raster format. These files will be combined into binned format
#'   data.
#'
#' @param save_prefix_name A string with a prefix that will be used name of file
#'   that contains the saved binned format data.
#'
#' @param bin_width A number that has the number of data samples that data will
#'   be averaged over.
#'
#' @param sampling_interval A number that has the specifies the sampling
#'   interval between successive binned data points.
#'
#' @param start_time A number that specifies the time to start binning the data.
#'   This needs to be set to one of the start times in the raster data; i.e., if
#'   data columns are in the format time.XXX_YYY, then the start_time must be
#'   one of the XXX values. By default, the start_time is the first
#'   time in the raster data.
#'
#' @param end_time A number that specifies the time to end the binning of the data.
#'   This needs to be set to one of the end times in the raster data; i.e., if
#'   data columns are in the format time.XXX_YYY, then the start_time must be
#'   one of the YYY values. By default, the end_time is the last
#'   time in the raster data.
#'
#' @param files_contain A string that specifies that only raster files that
#'   contain this string should be included in the binned format data.
#'   
#' @param num_parallel_cores An integer specifying the number of parallel cores
#'   to use. The default (NULL) value is to use half of the cores detected on
#'   the system. If this value is set to a value of less than 1, then the code
#'   will be run serially.
#'   
#' @return Returns a string with the name of the file that was created which has
#'   the data in binnned format.
#'   
#'
#' @examples
#' # create binned data with 150 ms bin sizes sampled at 10 ms intervals
#' raster_dir_name <- file.path(
#'   "..", "data-raw", "raster",
#'   "Zhang_Desimone_7objects_raster_data_rda", ""
#' )
#' 
#' raster_dir_name <- trimws(file.path(system.file("extdata", package = "NeuroDecodeR"), 
#'                           "Zhang_Desimone_7object_raster_data_small_rda", " "))
#' 
#' \donttest{
#' # The code could potentially run faster by using more parallel cores
#' # (e.g., by not setting the num_parallel_cores argument, half the cores available
#' #  will be used) 
#' binned_file_name <- create_binned_data(raster_dir_name, 
#'                                       file.path(tempdir(), "ZD"), 
#'                                       150, 50,
#'                                       num_parallel_cores = 2)
#' }
#'
#' @export
create_binned_data <- function(raster_dir_name,
                               save_prefix_name,
                               bin_width,
                               sampling_interval,
                               start_time = NULL,
                               end_time = NULL,
                               files_contain = "",
                               num_parallel_cores = NULL) {


  # if the directory name does not end with a slash, add a slash 
  raster_dir_name <- trimws(file.path(dirname(raster_dir_name), basename(raster_dir_name), " "))

  file_names <- list.files(raster_dir_name, pattern = files_contain)


  # if the num_parallel_cores is not set, use half the available cores
  #  or if that is more than the num_resample_runs, just use num_resample_runs cores
  
  if (is.null(num_parallel_cores)) {
    num_parallel_cores <- min(parallel::detectCores()/2, length(file_names))
  }
  
  
  if (num_parallel_cores > 0) {
    
    # register parallel resources
    the_cluster <- parallel::makeCluster(num_parallel_cores, 
                                         type = "SOCK")  # , parallel_outfile)
    doSNOW::registerDoSNOW(the_cluster)
    
    "%do_type%" <- get("%dopar%")
    
  } else {
    "%do_type%" <- get("%do%")
  }
  

  # Adding a pretty lame progress bar since unfortunately I couldn't get any of
  # the cooler progress bars (e.g., from the progress and progressr packages) to
  # work. On the plus side, the changes to the code are minimal.
  pb <- txtProgressBar(max = length(file_names), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)


  
  # Do a parallel loop through all raster data files and bin them
  iSite <- 0  # to deal with an R check note
  binned_data_list <- list()
  binned_data_list <- foreach(iSite = seq_along(file_names), .options.snow=opts) %do_type% { 
                                                 
    # print message to show progress the number of sites that have been binned    
    if (iSite == 1) {
      message(sprintf("binning site %-5s", iSite))
    } else {
      message(paste0(rep("\b", 19), collapse = ""), sprintf("binning site %-5s", iSite))
    }

    curr_file <- file_names[iSite]
    
    if (tolower(tools::file_ext(curr_file)) == "mat") {
      # doing this to avoid a message being printed that the read_matlab_raster_data() function should be used
      raster_data <- read_matlab_raster_data(file.path(raster_dir_name, curr_file))
    } else {
      raster_data <- NeuroDecodeR::read_raster_data(file.path(raster_dir_name, curr_file))
    }

    

    # If the end times in the raster data are not specified add them.
    #  i.e., if data columns are specified as time.XXX rather than time.XXX_YYY 
    #  add the YYY end time part to the time column names
    if (!check_raster_data_contains_end_times(raster_data)){
      raster_data <- add_raster_data_end_times(raster_data)
    } 
    

    
    one_binned_site <- bin_saved_data_one_site(raster_data, bin_width, sampling_interval, start_time, end_time)

    # append siteID to raster data, which is then appended to binned data
    one_binned_site$siteID <- rep(iSite, dim(one_binned_site)[1])
    binned_data_list[[iSite]] <- one_binned_site
    
  }

  
  # close parallel resources
  if (num_parallel_cores > 0) {
    parallel::stopCluster(the_cluster)
  }
  
  
  # initial way to combine list of data frames into a single data frame
  # binned_data <- do.call(rbind.data.frame, binned_data_list)
  
  # a more efficient way to combine a list of data frames into a single data frame 
  binned_data <- dplyr::bind_rows(binned_data_list)


  # make the siteID be in the first column of binned dataa
  binned_data <- binned_data %>% select(.data$siteID, everything())

  
  # add the class attributes binned_data, data.frame to the binned data
  attr(binned_data, "class") <- c("binned_data", "data.frame")
  
  
  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(
    save_prefix_name, "_", bin_width, "bins_", sampling_interval, "sampled")
  start_time_name <- ""
  end_time_name <- ""

  if (!is.null(start_time)) {
    start_time_name <- paste0("_start", start_time)
  }

  if (!is.null(end_time)) {
    end_time_name <- paste0("_end", end_time)
  }

  # if the file prefix includes a directory path that does not exist, create the directory 
  the_dir_name <- dirname(saved_binned_data_file_name)
  if (((!file.exists(the_dir_name)) && (the_dir_name != ""))) {
    message(paste0("The folder where the binned file should be saved does not exist. ",
            "Creating a new folder at ", the_dir_name , " to save the binned results."))
    dir.create(the_dir_name, recursive = TRUE) 
  }
  
  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_time_name, end_time_name, ".Rda")
  save("binned_data", file = saved_binned_data_file_name, compress = TRUE)

  saved_binned_data_file_name
} # end function






# A helper function for create_binned_data() to bin the data from one site
bin_saved_data_one_site <- function(raster_data,
                                    bin_width,
                                    sampling_interval,
                                    start_time = NULL,
                                    end_time = NULL) {

  labels_df <- dplyr::select(raster_data, -starts_with("time"))
  spike_df <- dplyr::select(raster_data, starts_with("time"))

  
  # start_df_ind is the index of the time column in spike_df
  # start_df_ind and start_time are the same if the time starts at 1

  # get vectors that have all the start times and end times of each rater time bin
  start_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[1]))
  end_time_values <- as.numeric(sapply(strsplit(gsub("time.", "", names(spike_df)), "_"), function(l) l[2]))
  
  # specify default start_time as the first bin if no start_time has been specified
  if (is.null(start_time)) {
    start_time <- start_time_values[1]  # alternatively could do min(start_time_values)
  }
  start_df_ind <- which(start_time_values == start_time)

  # check that a valid start_time has been specified
  if (length(start_df_ind) == 0) {
    stop(c("The specified start_time = ", start_time ," is not one of the start times listed in the raster data. One
           of the closest start time intervals is: ", names(spike_df)[which.min(abs(start_time_values - start_time))], 
           ". Perhaps use start_time = ", start_time_values[which.min(abs(start_time_values - start_time))], " ?"))
  }
  
  
  # specify default end_time as the last bin if no end_time has been specified
  if (is.null(end_time)) {
    end_time <- end_time_values[dim(spike_df)[2]]
  }
  end_df_ind <- which(end_time_values == end_time)
  
  
  # check that a valid end_time has been specified
  # check that a valid start_time has been specified
  if (length(end_df_ind) == 0) {
    stop(c("The specified end_time = ", end_time ," is not one of the end times listed in the raster data. One
           of the closest start time intervals is: ", names(spike_df)[which.min(abs(end_time_values - end_time))], 
           ". Perhaps use end_time = ", end_time_values[which.min(abs(end_time_values - end_time))], " ?"))
  }
  
  
  # bin the data!
  all_start_df_inds <- seq(start_df_ind, end_df_ind - (bin_width - 1), by = sampling_interval)
  all_end_df_inds <- all_start_df_inds + (bin_width - 1)
  dfCurr_site_binned_data <- as.data.frame(matrix(nrow = dim(raster_data)[1], ncol = length(all_start_df_inds)))

  for (iBin in seq_along(all_start_df_inds)) {

    if (all_start_df_inds[iBin] == all_end_df_inds[iBin]) {
      
      # if binning at the same resolution as the original file, return original data
      # add start_df_ind to offset the pre-stimulus time
      dfCurr_site_binned_data[, iBin] <- spike_df[, all_start_df_inds[iBin]]
      
    } else {

      # otherwise, actually bin the data
      dfCurr_site_binned_data[, iBin] <- rowMeans(spike_df[, all_start_df_inds[iBin]:all_end_df_inds[iBin]])

    }
  }

  
  names(dfCurr_site_binned_data) <- paste0("time.", start_time_values[all_start_df_inds], "_", end_time_values[all_end_df_inds])
  dfCurr_site_binned_data <- cbind(labels_df, dfCurr_site_binned_data)
  
  dfCurr_site_binned_data
}
